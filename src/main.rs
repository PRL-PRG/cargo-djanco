use std::fs::{File, create_dir_all};
use std::io::Read;
use std::path::PathBuf;
use std::path::Path;
use std::fmt::Display;
use std::fmt::Formatter;
use std::collections::VecDeque;
use std::collections::HashSet;
use std::str::FromStr;

extern crate proc_macro;

use syn::{self, Item, Attribute, ItemFn, ItemMod};
use toml;

use quote::ToTokens;

//use clap::Clap;
use anyhow::*;
use toml::Value;
use regex::Regex;
use chrono::{Datelike, TimeZone};
use proc_macro2::{TokenTree, TokenStream};
use itertools::Itertools;
use std::hash::{Hash, Hasher};

#[derive(Clone, Debug)]
struct QueryFunction {
    module: ModulePath,
    function: String,
    configuration: Configuration,
}

impl QueryFunction {
    pub fn from<S>(function_name: S, module: ModulePath, configuration: Configuration) -> Self where S: ToString {
        // println!("Found configuration {}::{} -> {:?}", module, function_name.to_string(), configuration);
        QueryFunction { function: function_name.to_string(), module, configuration }
    }
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

#[derive(Clone, Copy, Debug, Hash, PartialOrd, PartialEq, Ord, Eq)]
enum Month {
    January, February, March, April, May, June, July,
    August, September, October, November, December,
}

impl Month {
    pub fn as_u32(&self) -> u32 {
        match self {
            Month::January   => 1,
            Month::February  => 2,
            Month::March     => 3,
            Month::April     => 4,
            Month::May       => 5,
            Month::June      => 6,
            Month::July      => 7,
            Month::August    => 8,
            Month::September => 9,
            Month::October   => 10,
            Month::November  => 11,
            Month::December  => 12,
        }
    }
}

impl<T> From<&T> for Month where T: Datelike {
    fn from(datelike: &T) -> Self {
        match datelike.month() {
            1 => Month::January,
            2 => Month::February,
            3 => Month::March,
            4 => Month::April,
            5 => Month::May,
            6 => Month::June,
            7 => Month::July,
            8 => Month::August,
            9 => Month::September,
            10 => Month::October,
            11 => Month::November,
            12 => Month::December,
            i => panic!("Cannot convert number {} to Month", i),
        }
    }
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

impl Display for Month {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Month::January   => write!(f, "January"),
            Month::February  => write!(f, "February"),
            Month::March     => write!(f, "March"),
            Month::April     => write!(f, "April"),
            Month::May       => write!(f, "May"),
            Month::June      => write!(f, "June"),
            Month::July      => write!(f, "July"),
            Month::August    => write!(f, "August"),
            Month::September => write!(f, "September"),
            Month::October   => write!(f, "October"),
            Month::November  => write!(f, "November"),
            Month::December  => write!(f, "December"),
        }
    }
}

#[derive(Clone, Debug)]
enum Property {
    Month(Month),
    Year(Year),
    Seed(u128),
    Subsets(Vec<String>),
}

impl Property {
    fn year(year: u16)               -> Self { Property::Year(Year::from(year)) }
    fn month(month: Month)           -> Self { Property::Month(month)           }
    fn subsets(subsets: Vec<String>) -> Self { Property::Subsets(subsets)       }
    fn seed(seed: u128)              -> Self { Property::Seed(seed)             }

    fn interpret_literal<S>(literal: S) -> Self where S: ToString {
        if let Some(year) = literal.as_year() {
            Property::year(year)
        } else {
            panic!("Unknown property: {}", literal.to_string())
        }
    }

    fn interpret_indentifier<S>(identifier: S) -> Self where S: ToString {
        if let Some(month) = identifier.as_month() {
            Property::month(month)
        } else {
            panic!("Unknown property: {}", identifier.to_string())
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
        let string_regex = Regex::new(r#"^"(?P<l>.*)"$"#).unwrap();

        for token in self {
            match token {
                TokenTree::Group(group) if preceding_identifier.is_some() => {
                    let name = preceding_identifier.unwrap();
                    if name.as_month().is_some() {
                        panic!("Month {} cannot have arguments {:?}", name, group.to_string());
                    }
                    let property = match name.to_lowercase().as_str() {
                        "subset" | "subsets" => {
                            let subsets = group.stream().into_iter().flat_map(|member| match member {
                                TokenTree::Group(_) => {
                                    panic!("Property {} can only contain subset definitions.", name)
                                },
                                TokenTree::Ident(identifier) => {
                                    Some(identifier.to_string())
                                },
                                TokenTree::Punct(_) => {
                                    None
                                },
                                TokenTree::Literal(literal) => {
                                    let literal = literal.to_string();
                                    if !string_regex.is_match(&literal) {
                                        Some(literal)
                                    } else {
                                        let unquoted = &string_regex.replace_all(&literal, "$l");
                                        let unescaped = unescape::unescape(unquoted).unwrap();
                                        Some(unescaped)
                                    }
                                },
                            }).collect();
                            Property::subsets(subsets)
                        },
                        "seed" => {
                            let seed: Vec<u128> = group.stream().into_iter()
                                .map(|tokens| tokens.to_string())
                                .map(|string| u128::from_str(string.as_str()).unwrap())
                                .collect();
                            if seed.len() < 1 {
                                panic!("Expected seed to contain one numeric argument, but found none");
                            }
                            if seed.len() > 1 {
                                panic!("Expected seed to contain one numeric argument, but found {}", seed.len());
                            }
                            Property::seed(*seed.first().unwrap())
                        },
                        _ => {
                            panic!("Djanco does not allow property {}.", name);
                        }
                    };
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

#[derive(Clone, Copy, Debug, Hash, PartialOrd, PartialEq, Ord, Eq)]
struct Year(u16);

impl Year {
    pub fn as_i32(&self) -> i32 {
        self.0 as i32
    }
}

impl From<u16> for Year {
    fn from(n: u16) -> Self {
        if n < 2005 || n > 2035 {
            panic!("{} is not a valid year (should be between {} and {})", n, 2005, 2035);
        }
        Year(n)
    }
}

impl<T> From<&T> for Year where T: Datelike {
    fn from(datelike: &T) -> Self {
        Year(datelike.year() as u16)
    }
}

impl Display for Year {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Configuration {
    month: Month,
    year: Year,
    subsets: HashSet<String>,
    seed: u128,
}

impl Hash for Configuration {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.month.hash(state);
        self.year.hash(state);
        self.seed.hash(state);
        self.subsets.iter().collect::<Vec<&String>>().into_iter().for_each(|e| e.hash(state))
    }
}

impl From<Vec<Property>> for Configuration {
    fn from(parameters: Vec<Property>) -> Configuration {
        let mut month: Option<Month> = None;
        let mut year: Option<Year> = None;
        let mut subsets: HashSet<String> = HashSet::new();
        let mut seed: Option<u128> = None;

        for parameter in parameters {
            match parameter {
                Property::Month(value) if month.is_some() => {
                    panic!("Attempt to define multiple months in one `djanco` tag ({} and {}...). \
                            If you want to run one query at multiple savepoints, define another \
                            `djanco` tag for the same function.",
                           month.unwrap(), value);
                }
                Property::Year(value) if year.is_some() => {
                    panic!("Attempt to define multiple years in one `djanco` tag ({} and {}...). \
                            If you want to run one query at multiple savepoints, define another \
                            `djanco` tag for the same function.",
                           year.unwrap(), value);
                }
                Property::Seed(value) if seed.is_some() => {
                    panic!("Attempt to define multiple seeds in one `djanco` tag ({} and {}...). \
                            If you want to run one query with multiple random seeds, define \
                            another `djanco` tag for the same function.",
                           seed.unwrap(), value);
                }
                Property::Month(value) => { month = Some(value); }
                Property::Year(value)  => { year  = Some(value); }
                Property::Seed(value)  => { seed  = Some(value); }
                Property::Subsets(vector) => {
                    subsets.extend(vector.into_iter())
                }
            }
        }

        let today = chrono::Local::today();
        let month: Month = match (&year, month) {
            (_, Some(month)) => month,
            (Some(_year), None) => Month::January,
            (None, None) => Month::from(&today),
        };
        let year: Year = if let Some(year) = year { year } else { Year::from(&today) };
        //let subsets: Vec<String> = ...
        let seed: u128 = if let Some(seed) = seed { seed } else { 0 };

        Configuration { month, year, subsets, seed }
    }
}

impl From<Attribute> for Configuration {
    fn from(attribute: Attribute) -> Self {
        let identifier: String = attribute.path.get_ident().unwrap().to_string();
        let tokens: Vec<TokenTree> = attribute.tokens.into_token_stream().into_iter().collect();

        if tokens.len() > 1 {
            panic!("Attribute expected to have zero or one argument group, but found {}", tokens.len());
        }
        let arguments: Vec<Property> = match tokens.into_iter().last() {
            None => Vec::new(),
            Some(TokenTree::Group(group)) => {
                let tokens = group.stream();
                tokens.into_properties()
            },
            Some(thing) => panic!("Expecting an argument group for argument {}, but found {:?}",
                                  identifier, thing)
        };
        Configuration::from(arguments)
    }
}

fn evaluate_function(function: ItemFn, module: &ModulePath, found_queries: &mut Vec<QueryFunction>) -> Result<()> {
    //println!("Analyzing function: {}", function.sig.ident.to_string());
    let query_configurations = function.get_djanco_attributes().into_iter()
        .map(|attribute| {
            Configuration::from(attribute)
        })
        .map(|configuration| {
            QueryFunction::from(function.sig.ident.to_string(), module.clone(), configuration)
        });
    found_queries.extend(query_configurations);
    Ok(())
}

fn evaluate_item(item: &Item, module_path: &ModulePath, found_queries: &mut Vec<QueryFunction>) -> Result<()> {
    match item {
        Item::Fn(function) => evaluate_function(function.clone(), module_path, found_queries),
        Item::Mod(module) => evaluate_module(&module, module_path, found_queries),
        //Item::ForeignMod(module) => {} // extern
        _ => Ok(()),
    }
}

fn evaluate_module(module: &ItemMod, module_path: &ModulePath, found_queries: &mut Vec<QueryFunction>) -> Result<()> {
    //println!("Analyzing module: {}", module.ident.to_string());
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
    // println!("Analyzing source file: {:?}", path);

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

const USES: [&'static str; 4] = ["djanco::*", "djanco::log::*", "djanco::utils::*", "clap::Parser"];

const MAIN_HEADER: &'static str = r#"
    let options = Configuration::parse();
    let log = Log::new(options.verbosity);
    let dataset = options.dataset_path_as_str();
    let cache = options.cache_path_as_str();

    let repository = if let Some(repository) = options.repository.as_ref() {
        Some(create_project_archive(PROJECT_NAME, repository.as_str()))
    } else {
        None
    };

    macro_rules! execute_query {
        ($database:expr, $method:path) => {
            timed_query!($method[&$database, &log, &options.output_path]);
        }
    }

    macro_rules! prepare_database {
        ($savepoint:expr, $stores:expr) => {
            Djanco::from_config(&options, $savepoint, $stores, log.clone())
                .expect("Error initializing Djanco!");
        }
    }
"#;

const MAIN_FOOTER: &'static str = r#"
    if options.repository.is_some() && !options.do_not_archive_results {
        add_results(PROJECT_NAME, &repository.unwrap(), &options.output_path, options.size_limit);
    }
"#;

fn generate_code (project_name: String, queries: Vec<QueryFunction>) -> String {
    let mut code = String::new();

    for package in USES.iter() {
        code.push_str(&format!("use {};\n", package));
    }
    code.push('\n');

    code.push_str(&format!("use {};\n", project_name));
    code.push('\n');

    code.push_str(&format!("const PROJECT_NAME: &'static str = \"{}\";\n", project_name));
    code.push('\n');

    code.push_str("pub fn main() {\n");
    code.push_str(MAIN_HEADER);
    code.push('\n');

    let query_groups = queries.into_iter()
        .map(|query| (query.configuration.clone(), query))
        .into_group_map().into_iter();

    for (configuration, queries) in query_groups {
        code.push_str("    ");
        code.push_str(configuration.to_source().as_str());
        for query in queries {
            code.push_str("    ");
            code.push_str(query.to_source().as_str());
        }
        code.push('\n');
    }

    code.push_str(MAIN_FOOTER);
    code.push_str("}\n");
    code
}


pub trait ToSource {
    const INDENT: &'static str = "    ";
    fn to_source(&self) -> String; // FIXME write out to object
    // fn to_source_with_indent(&self) -> String {
    //     self.to_source().split("\n").map(|s| format!("{}{}", Self::INDENT, s)).join("\n")
    // }
}

impl ToSource for Configuration {
    fn to_source(&self) -> String {
        let timestamp = chrono::Utc.ymd(self.year.as_i32(), self.month.as_u32(), 1)
                .and_hms(0, 0, 0)
                .timestamp();

        let stores =
            if self.subsets.is_empty() { "All".to_owned() } else { self.subsets.iter().join(", ") };

        format!("let database = prepare_database!({} /* = {} {}*/, stores!({}));\n",
                timestamp, self.month, self.year, stores)
    }
}

impl ToSource for QueryFunction {
    fn to_source(&self) -> String {
        format!("execute_query!(database, {});\n", self)
    }
}

fn main() {
    //let options = CommandLineOptions::parse(); // Populate config from commandline arguments.
    let manifest = load_manifest("Cargo.toml")
        .expect("Manifest file not found at ./Cargo.toml");
    let crate_name = manifest.get_crate_name()
            .expect("Could not find crate name in manifest")
            .replace("-", "_");
    let lib_path = PathBuf::from(manifest.get_lib_path()
        .expect("Error reading lib path from manifest")
        .unwrap_or("src/lib.rs".to_owned()));

    println!("Creating a runner for crate `{}`", crate_name);
    // println!("lib path: {:?} -> {}", lib_path, lib_path.exists());

    let module_path = &ModulePath::root(crate_name.clone());
    let mut found_queries = Vec::new();
    evaluate_source_file(&lib_path, module_path, &mut found_queries).unwrap();

    let source_code = generate_code(crate_name, found_queries);

    //println!("{}", source_code);
    println!("Generating a runner at `src/bin/djanco.rs`");
    create_dir_all("src/bin/").unwrap();
    std::fs::write("src/bin/djanco.rs", source_code).unwrap();

    println!("Execute runner using the following cargo command:");
    println!();
    println!("    cargo run --bin djanco --release -- --output-path PATH --dataset-path PATH");
    println!();
}
