# Djanco query execution harness

`cargo-djanco` is a helper tool that generates a harness for executing 
![Djanco](https://github.com/PRL-PRG/djanco/) queries.

The tool provides a cargo command (`djanco`). Executing it will analyze the 
current crate looking for functions 
![marked as queries](https://github.com/PRL-PRG/djanco_ext) and generate a Rust
source file that will set the queries up and execute each of them.

# Instalation and running

Install from within your cargo crate:

```bash
cargo install --git https://github.com/PRL-PRG/cargo-djanco
```

Generate harness:

```bash
cargo djanco
```

Execute your queries:

```bash
cargo --bin djanco --release -- \
    --dataset-path DATASET_LIVES_HERE 
    --output-path WRITE_RESULTS_HERE 
``` 

# Queries

`cargo-djanco` is used as a tool within an existing Rust crate containing some
queries. The crate should depend on 
![djanco_ext](https://github.com/PRL-PRG/djanco_ext). Queries are functions 
tagged with as `djanco`. Example:

```rust
#[djanco(April, 2020, subsets(C, Python, SmallProjects))]
pub fn my_query(database: &Database, _log: &Log, output: &Path) -> Result<(), std::io::Error>  {
    database.projects()
        .group_by(project::Language)
        .sort_by(project::Stars)
        .sample(Top(1000))
        .into_csv_in_dir(output, "top_1000_by_stars.csv")
}
``` 

The queries are functions with any name, three arguments, and that return a 
`Result` type. They can be located in any module in the crate. The arguments 
represent:
 
 - a reference to a database (`&djanco::data::Database`) where the query should
   get its data from,
 - a log (`djanco::log::Log`) where the query will write runtime logs into,
 - output path (`std::path::Path`) of a directory where the query should write
   its results.
 
The tag has several arguments as well:
 - Two describe a savepoint timestamp at which the database is checked out: 
   month and year, both optional. If they are not provided, the current month
   and/or year are used.
 - `subset`/`subsets` describes which subsets of the datastore to use in the 
   query. If not provided, all subsets will be checked out.  

# Generating a harness

This cargo command analyzes the crate, extracts the queries and generates a harness:

```bash
cargo djanco
```

The harness is a single source file in `src/bin/djanco.rs`. It can be compiled
and executed. It contains boilerplate code for running queries, measuring their
execution times, and archiving them to a git repository.

Queries are grouped by their database parameters. For each group code is 
emitted that sets up the database and executes all queries that use this 
specific configuartion.

```rust
let database = prepare_database!(1585699200 /* = April 2020*/, stores!(C, Python, SmallProjects));
execute_query!(database, my_crate::my_query1);
execute_query!(database, my_crate::my_query3);
execute_query!(database, my_crate::my_query2);
```

The order in which the harness will try to execute queries is not predictable.

A query can be executed as part of multiple groups if it was tagged with 
multiple configurations.

# Archiving a harness

The whole crate can be archived on an archive repository. To do this, specify
a repository when executing the harness:

```bash
cargo --bin djanco --release -- \
    --dataset-path DATASET_LIVES_HERE 
    --output-path WRITE_RESULTS_HERE 
    --repository REPOSITORY_URL
```

This will check out a branch of the same name as the current crate, and push
the entirety of the current crate's code as a single commit. Then, after the 
query is executed, the result directory will be pushed as a second commit.

It's not always wise to store the results of queries, since they can get quite
big. This step can therefore be skipped:

```bash
cargo --bin djanco --release -- \
    --dataset-path DATASET_LIVES_HERE 
    --output-path WRITE_RESULTS_HERE 
    --repository REPOSITORY_URL
    --skip-results 
```
