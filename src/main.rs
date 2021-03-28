use std::fs::File;
use std::io::{Read, stdout};
use std::path::{PathBuf, Path};
use std::fmt::{Display, Formatter};
use std::collections::VecDeque;

use syn::{self, Item, Attribute, ItemFn, ItemMod};
use toml;

use quote::{quote, ToTokens, quote_spanned};
use quote::format_ident;

//use clap::Clap;
use anyhow::*;
use toml::Value;
use std::process::Command;
use quote::__private::{TokenStream, TokenTree, Literal};

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

trait DjancoProperty {
    fn tagged_as_djanco(&self) -> bool;
    fn get_djanco_attributes(&self) -> Vec<Attribute>;
}

impl DjancoProperty for Attribute {
    fn tagged_as_djanco(&self) -> bool {
        self.path.get_ident().map_or(false, |ident| ident.to_string().as_str() == "djanco")
    }

    fn get_djanco_attributes(&self) -> Vec<Attribute> {
        if self.tagged_as_djanco() {
            vec![self.clone()]
        } else {
            vec![]
        }
    }
}

impl DjancoProperty for ItemFn {
    fn tagged_as_djanco(&self) -> bool {
        self.attrs.iter().any(|attribute| attribute.tagged_as_djanco())
    }
    fn get_djanco_attributes(&self) -> Vec<Attribute> {
        self.attrs.iter().flat_map(|attribute| attribute.get_djanco_attributes()).collect()
    }
}

fn is_tagged_as_djanco(attributes: &Vec<Attribute>) -> bool {
    attributes.iter().any(|attribute| attribute.tagged_as_djanco())
}

fn as_month<S>(str: S) -> Option<(String, u8)> where S: Into<String> {
    match str.into().to_lowercase().as_str() {
        "january"   | "jan" => Some(("January",   1)),
        "february"  | "feb" => Some(("February",  2)),
        "march"     | "mar" => Some(("March",     3)),
        "april"     | "apr" => Some(("April",     4)),
        "may"               => Some(("May",       5)),
        "june"      | "jun" => Some(("June",      6)),
        "july"      | "jul" => Some(("July",      7)),
        "august"    | "aug" => Some(("August",    8)),
        "september" | "sep" => Some(("September", 9)),
        "october"   | "oct" => Some(("October",  10)),
        "november"  | "nov" => Some(("November", 11)),
        "december"  | "dec" => Some(("December", 12)),
        _                   => None,
    }.map(|(s, n)| (s.to_string(), n))
}

trait AsYear {
    fn as_year(&self) -> Option<u16>;
}

impl<S> AsYear for S where S: ToString {
    fn as_year(&self) -> Option<u16> {
        self.to_string().parse::<u16>().map_or(None, |number|
           if number >= 2005 && number <= 2050 {
               Some(number)
           } else {
               None
           })
    }
}
//
// fn as_year(&self) -> Option<u16> {
//     self.into().parse::<u16>().map_or(None, |number|
//         if number >= 2005 && number <= 2050 {
//             Some(number)
//         } else {
//             None
//         })
// }

#[derive(Clone, Copy, Debug)]
enum Month {
    January, February, March, April, May, June, July,
    August, September, October, November, December,
}

trait AsMonth {
    fn as_month(&self) -> Option<Month>;
}

impl<S> AsMonth for S where S: ToString {
    fn as_month(&self) -> Option<Month> {
        match self.to_string().to_lowercase().as_str() {
            "january"   | "jan" => Some(Month::January),
            "february"  | "feb" => Some(Month::February),
            "march"     | "mar" => Some(Month::March),
            "april"     | "apr" => Some(Month::April),
            "may"               => Some(Month::May),
            "june"      | "jun" => Some(Month::June),
            "july"      | "jul" => Some(Month::July),
            "august"    | "aug" => Some(Month::August),
            "september" | "sep" => Some(Month::September),
            "october"   | "oct" => Some(Month::October),
            "november"  | "nov" => Some(Month::November),
            "december"  | "dec" => Some(Month::December),
            _                   => None,
        }
    }
}

#[derive(Clone, Debug)]
enum Property {
    Month(Month),
    Year(u16),
    Entity { name: String, arguments: Vec<Property> },
    Literal(String),
}

impl Property {
    fn year(year: u16) -> Self {
        Property::Year(year)
    }

    fn month(month: Month) -> Self {
        Property::Month(month)
    }

    fn identifier(name: String) -> Self {
        Property::Entity { name, arguments: vec![] }
    }

    fn entity(name: String, arguments: Vec<Property>) -> Self {
        Property::Entity { name, arguments }
    }

    fn literal(value: String) -> Self {
        Property::Literal(value)
    }

    fn interpret_literal<S>(literal: S) -> Self where S: ToString {
        if let Some(year) = literal.as_year() {
            Property::year(year)
        } else {
            Property::literal(literal.to_string())
        }
    }

    fn interpret_indentifier<S>(identifier: S) -> Self where S: ToString {
        if let Some(month) = identifier.as_month() {
            Property::month(month)
        } else {
            Property::literal(identifier.to_string())
        }
    }
}

trait IntoPropertyVector {
    fn into_properties(self) -> Vec<Property>;
}

impl IntoPropertyVector for TokenStream {
    fn into_properties(self) -> Vec<Property> {
        let mut properties = vec![];
        let mut preceding_identifier: Option<String> = None;

        for token in self {
            match token {
                TokenTree::Group(group) if preceding_identifier.is_some() => {
                    let name = preceding_identifier.unwrap();
                    if name.as_month().is_some() {
                        panic!("Month {} cannot have arguments {:?}", name, group.to_string());
                    }
                    let arguments = group.stream().into_properties();
                    let property = Property::entity(name, arguments);
                    properties.push(property);
                    preceding_identifier = None;
                }
                TokenTree::Group(group) => {
                    panic!("Unnamed argument group: {:?}", group.to_string())
                }
                TokenTree::Ident(identifier) if preceding_identifier.is_some() => {
                    properties.push(Property::interpret_indentifier(preceding_identifier.unwrap()));
                    preceding_identifier = Some(identifier.to_string());
                }
                TokenTree::Ident(identifier) => {
                    preceding_identifier = Some(identifier.to_string());
                }
                TokenTree::Literal(literal) if preceding_identifier.is_some() => {
                    properties.push(Property::interpret_indentifier(preceding_identifier.unwrap()));
                    properties.push(Property::interpret_literal(literal.to_string()));
                    preceding_identifier = None;
                }
                TokenTree::Literal(literal) => {
                    properties.push(Property::interpret_literal(literal.to_string()));
                }
                TokenTree::Punct(_) => {/* ignore */}
            }
        }

        if let Some(identifier) = preceding_identifier {
            properties.push(Property::interpret_indentifier(identifier));
        }

        properties
    }
}

impl From<Attribute> for Property {
    fn from(attribute: Attribute) -> Self {
        let identifier: String = attribute.path.get_ident().unwrap().to_string();
        let tokens: Vec<TokenTree> = attribute.tokens.into_iter().collect();

        if tokens.len() != 1 {
            panic!("Attribute expected to have one argument group, but found {}", tokens.len());
        }
        let arguments: Vec<Property> = match tokens.into_iter().last().unwrap() {
            TokenTree::Group(group) => {
                let tokens = group.stream();
                tokens.into_properties()
            },
            thing => panic!("Expecting an argument group for argument {}, but found {:?}",
                            identifier, thing)
        };
        Property::entity(identifier, arguments)
    }
}

fn evaluate_function(function: &ItemFn, module: &ModulePath, found_queries: &mut Vec<QueryFunction>) -> Result<()> {
    println!("Analyzing function: {}", function.sig.ident.to_string());
    // if function.tagged_as_djanco() {
    let selected_attributes = function.get_djanco_attributes();
    //println!("selected attributes: {:?}", selected_attributes);
    for attribute in selected_attributes {
        let property = Property::from(attribute);
        println!("property: {:?}", property);

        // FIXME

        found_queries.push(QueryFunction {
            function: function.sig.ident.to_string(),
            module: module.clone()
        });
    }
    // }
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
    println!("Analyzing module: {}", module.ident.to_string());

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
    println!("Analyzing source file: {:?}", path);

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

fn generate_code (project_name: String, queries: Vec<QueryFunction>) -> String {
    let project = format_ident!("{}", project_name);
    let tokens = quote! {
        use clap::Clap;

        use djanco::*;
        use djanco::log::*;
        use djanco::utils::*;

        use #project;

        // These are automatically generated for the crate.
        const PROJECT_NAME: &'static str = #project_name;
        const SAVEPOINT: i64 = 1606780800; // 1st December 2020 // FIXME
        const SUBSTORES: [Store; 1] = [Store::Large(store::Language::JavaScript)]; // FIXME

        pub fn main() {
            let options = CommandLineOptions::parse();

            let repository = if let Some(repository) = options.repository.as_ref() {
                Some(create_project_archive(PROJECT_NAME, repository.as_str()))
            } else {
                None
            };

            let log = Log::new(options.verbosity);
            let database = Djanco::from_spec(
                options.dataset_path_as_str(),
                options.cache_path_as_str(),
                SAVEPOINT,
                SUBSTORES.iter().map(|store| store.clone()).collect(),
                log.clone()
            ).expect("Error initializing Djanco!");

            macro_rules! execute_query {
                ($method:path) => {
                    timed_query!($method[&database, &log, &options.output_path]);
                }
            }

            // These are automatically generated for the crate.
            init_timing_log!();
            execute_query!(djanco_template::hello_world);
            execute_query!(djanco_template::inner::hello_world);
            execute_query!(djanco_template::mymod::queryrrr);
            execute_query!(djanco_template::butts::xxxx1);
            execute_query!(djanco_template::butts::xxxx2);
            execute_query!(djanco_template::butts::butter::not_omitted);
            execute_query!(djanco_template::butts::butter::xxxx);

            if options.repository.is_some() && !options.do_not_archive_results {
                add_results(PROJECT_NAME, &repository.unwrap(), &options.output_path, options.size_limit);
            }
        }
    };

    tokens.to_string()
}

fn write_into<P, S>(path: P, code: S) -> anyhow::Result<()> where P: AsRef<Path>, S: ToString {
    std::fs::write(&path, code.to_string())?;
    Command::new("rustfmt")
        .arg(path.as_ref())
        .spawn()?
        .wait()?;
    Ok(())
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

    let module_path = &ModulePath::root(crate_name.clone());
    let mut found_queries = Vec::new();
    evaluate_source_file(&lib_path, module_path, &mut found_queries).unwrap();

    let tokens = generate_code(crate_name, found_queries);

    //println!("{}", tokens.into_token_stream().to_string());
    write_into("test.rs", tokens);
}
