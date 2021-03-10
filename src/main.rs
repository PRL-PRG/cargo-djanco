use std::fs::File;
use std::io::Read;
use std::path::{PathBuf, Path};
use std::fmt::{Display, Formatter};
use std::collections::VecDeque;

use syn::{self, Item, Attribute, ItemFn, ItemMod};
use toml;

//use clap::Clap;
use anyhow::*;
use toml::Value;

// #[derive(Clap)]
// #[clap(version = "1.0", author = "Konrad Siek <konrad.siek@gmail.com>")]
// struct CommandLineOptions {
//     // #[clap(short = 'o', long = "output-path", alias = "output-dir", parse(from_os_str))]
//     // pub output_path: Option<PathBuf>,
//     //
//     // #[clap(name="FILE", parse(from_os_str))]
//     // pub inputs: Vec<PathBuf>,
//     //
//     // #[clap(long = "as-json")]
//     // pub json: bool,
//     //
//     // #[clap(long = "as-yaml")]
//     // pub yaml: bool,
//     //
//     // #[clap(long = "as-sexpr", alias = "as-lisp")]
//     // pub lisp: bool,
//
//     //#[structopt(short = "f", long = "force")]
//     //pub force: bool,
// }

#[derive(Clone, Debug)]
struct QueryFunction {
    module: ModulePath,
    function: String,
}

impl Display for QueryFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.module, self.function)
    }
}

#[derive(Clone, Debug)]
struct ModulePath(Vec<String>);

impl ModulePath {
    pub fn root<S>(package: S) -> ModulePath where S: Into<String> {
        ModulePath(vec![package.into()])
    }
    pub fn enter<S>(&self, module: S) -> Self where S: Into<String> {
        let mut other = self.clone();
        other.0.push(module.into());
        other
    }
    pub fn as_path_buf(&self) -> PathBuf {
        let mut path_vec: VecDeque<String> = VecDeque::from(self.0.clone());
        path_vec.pop_front().unwrap();
        path_vec.push_front("src".to_string());
        PathBuf::from(Vec::from(path_vec).join("/"))
    }
}

impl Display for ModulePath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.join("::"))
    }
}

fn is_tagged_as_djanco(attributes: &Vec<Attribute>) -> bool {
    attributes
        .iter()
        .flat_map(|attribute| {
            attribute.path.get_ident().map(|ident| ident.to_string())
        })
        .any(|tag| tag == "djanco")
}

fn evaluate_function(function: &ItemFn, module: &ModulePath, found_queries: &mut Vec<QueryFunction>) -> Result<()> {
    if is_tagged_as_djanco(&function.attrs) {
        found_queries.push(QueryFunction {
            function: function.sig.ident.to_string(),
            module: module.clone()
        });
    }
    Ok(())
}

fn evaluate_item(item: &Item, module_path: &ModulePath, found_queries: &mut Vec<QueryFunction>) -> Result<()> {
    match item {
        Item::Fn(function) => evaluate_function(&function, module_path, found_queries),
        Item::Mod(module) => evaluate_module(&module, module_path, found_queries),
        //Item::ForeignMod(module) => {} // extern
        _ => Ok(()),
    }
}

fn evaluate_module(module: &ItemMod, module_path: &ModulePath, found_queries: &mut Vec<QueryFunction>) -> Result<()> {
    let module_path = module_path.enter(module.ident.to_string());

    if let Some((_, items)) = &module.content {
        for item in items {
            evaluate_item(&item, &module_path, found_queries)?
        }
        return Ok(());
    }

    let module_dir = module_path.as_path_buf();
    let module_file = if module_dir.is_dir() {
        let mut module_file = module_dir.clone();
        module_file.push("mod");
        module_file.set_extension("rs");
        if !module_file.exists() {
            bail!("Expected directory {} to contain the file `mod.rs`, but no such file exists",
                  module_dir.to_string_lossy());
        }
        module_file
    } else {
        let mut module_file = module_dir.clone();
        module_file.set_extension("rs");
        if !module_file.exists() {
            bail!("Expected file {} or directory {} to exist, but they do not",
              module_file.to_string_lossy(),
              module_dir.to_string_lossy());
        }
        module_file
    };

    return evaluate_source_file(module_file.as_path(), &module_path, found_queries);
}

fn evaluate_source_file(path: &Path, module_path: &ModulePath, found_queries: &mut Vec<QueryFunction>) -> Result<()> {
    let mut source_file = File::open(path)?;
    let mut source_code = String::new();
    source_file.read_to_string(&mut source_code)?;

    let ast = syn::parse_file(&source_code)?;
    for item in ast.items {
        evaluate_item(&item, module_path, found_queries)?
    }
    Ok(())
}

fn load_manifest<S>(path: S) -> Result<Value> where S: Into<PathBuf> {
    let mut manifest_file = File::open(path.into())?;
    let mut manifest_source = String::new();
    manifest_file.read_to_string(&mut manifest_source)?;
    Ok(manifest_source.parse::<Value>()?)
}

trait Manifest {
    fn get_crate_name(&self) -> Result<String>;
    fn get_lib_path(&self) -> Result<Option<String>>;
}

fn value_to_string(value: &Value) -> String {
    match value {
        Value::String(_) => "string",
        Value::Integer(_) => "integer",
        Value::Float(_) => "float",
        Value::Boolean(_) => "boolean",
        Value::Datetime(_) => "datetime",
        Value::Array(_) => "array",
        Value::Table(_) => "table",
    }.to_owned()
}

impl Manifest for Value {
    fn get_crate_name(&self) -> Result<String> {
        let table = match self {
            Value::Table(table) => table,
            _ => bail!("Expected a table, found a {}: {:?}", value_to_string(self), self),
        };

        let package = table.get("package");
        if package.is_none() {
            bail!("Could not find key `package` in table")
        }
        let package = package.unwrap();
        let package = match package {
            Value::Table(table) => table,
            _ => bail!("Expected a `package` table, found a {}: {:?}", value_to_string(package), package),
        };

        let name = package.get("name");
        if name.is_none() {
            bail!("Could not find key `name` in table `package`")
        }
        let name = name.unwrap();
        if let Value::String(name) = name {
            return Ok(name.to_owned());
        }
        bail!("Expected a string, found a {}: `{:?}`", value_to_string(name), name)
    }

    fn get_lib_path(&self) -> Result<Option<String>> {
        let table = match self {
            Value::Table(table) => table,
            _ => bail!("Expected a table, found a {}: {:?}", value_to_string(self), self),
        };

        let lib = table.get("lib");
        if lib.is_none() {
            //bail!("Could not find key `lib` in table")
            return Ok(None)
        }
        let lib = lib.unwrap();
        let lib = match lib {
            Value::Table(table) => table,
            _ => bail!("Expected a `lib` table, found a {}: {:?}", value_to_string(lib), lib),
        };

        let path = lib.get("path");
        if path.is_none() {
            //bail!("Could not find key `path` in table `lib`")
            return Ok(None)
        }
        let path = path.unwrap();
        if let Value::String(path) = path {
            return Ok(Some(path.to_owned()))
        }
        bail!("Expected a string, found a {}: `{:?}`", value_to_string(path), path)
    }
}

fn main() {
    //let options = CommandLineOptions::parse(); // Populate config from commandline arguments.
    let manifest = load_manifest("Cargo.toml")
        .expect("Manifest file not found at ./Cargo.toml");
    let crate_name = manifest.get_crate_name()
            .expect("Could not find crate name in manifest");
    let lib_path = PathBuf::from(manifest.get_lib_path()
        .expect("Error reading lib path from manifest")
        .unwrap_or("src/lib.rs".to_owned()));

    println!("crate name: {:?}", crate_name);
    println!("lib path: {:?} -> {}", lib_path, lib_path.exists());

    let module_path = &ModulePath::root(crate_name);
    let mut found_queries = Vec::new();
    evaluate_source_file(&lib_path, module_path, &mut found_queries).unwrap();

    for query in found_queries {
        println!("{}", query)
    }
}
