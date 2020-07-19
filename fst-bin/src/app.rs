const ABOUT: &str = "\
A command line tool for building, searching and inspecting FSTs.
";

const ABOUT_CSV: &str = "\
Emit information in CSV format about the transducer.

If <output> is not set, then CSV data is emitted to stdout.
";

const ABOUT_DOT: &str = "\
Emit this transducer in the \"dot\" format.

If <output> is not set, then the \"dot\" description is emitted to stdout.

Generally, usage of this command should look like this:

    $ fst dot your-transducer.fst | dot -Tpng > your-transducer.png
    $ $YOUR_FAVORITE_IMAGE_VIEWER your-transducer.png

Where 'dot' is a command line utility that is part of graphviz.

If your transducer contains output values, then they are shown as labels on
transitions. Zero output values are omitted.
";

const ABOUT_DUPES: &str = "\
A simple way to show duplicate nodes.

This is meant to be a diagnostic tool to view duplicate nodes in the
transducer. Every duplicate node represents a missed opportunity for more
compression. A minimal transducer should have precisely zero duplicate nodes.

WARNING: This stores all nodes in the transducer in memory, decompressed. This
may be expensive in both time and space depending on the size of your
transducer.

If <output> is omitted, then diagnostic data is emitted to stdout.
";

const ABOUT_FUZZY: &str = "\
Issues a fuzzy query against the given transducer.

A fuzzy query returns all search results within a particular edit distance
of the query given.

WARNING: This works by building a Levenshtein automaton, which is currently
rather expensive (in time and space) with a big edit distance. This will be
improved in the future.
";

const ABOUT_GREP: &str = "\
Searches a transducer with a regular expression.

WARNING: This works by building a regular expression automaton, which can be
quite expensive depending on how big the regex is. If this becomes a problem,
consider disabling Unicode support in the regex via '(?-u)'.
";

const ABOUT_MAP: &str = "\
Creates an ordered map backed by a finite state transducer.

The input to this command should be a CSV file with exactly two columns and no
headers. The first column should be the key and the second column should be a
value that can be interpreted as an unsigned 64 bit integer.

If your input is already sorted, then pass the --sorted flag to make
construction much faster. If you use --sorted and the data is not sorted,
then this will return an error when it sees an out-of-order key.
";

const ABOUT_NODE: &str = "\
Shows a single node from the transducer.

The input to this command is the node's address. An address may be found either
from debugging a transducer in code, or from the output of the 'fst csv'
command.

If the address does not point to a valid node, then the executable may panic or
abort without ceremony.
";

const ABOUT_RANGE: &str = "\
Issues a range query against the given transducer.

A range query returns all search results within a particular.

If neither the start or the end of the range is specified, then all entries
in the transducer are shown.
";

const ABOUT_RUST: &str = "\
Emit Rust source code for the given FST.

This reads the FST given and emits it as Rust source code with one
constant defined:

    {NAME}_BYTES

And a `lazy_static!` ref for:

    {NAME}

Where {NAME} is taken from the name given as an argument.

The latter definition corresponds to calling `Fst::new({NAME}_BYTES)`.
This makes it possible to trivially use pre-built FSTs in your program.
";

const ABOUT_SET: &str = "\
Creates an ordered set backed by a finite state transducer.

The input to this command should be one or more files with one key per line.

If your input is already sorted, then pass the --sorted flag to make
construction much faster. If you use --sorted and the data is not sorted,
then this will return an error when it sees an out-of-order key.
";

const ABOUT_UNION: &str = "\
Unions all of the transducer inputs into a single transducer.

Any output values are dropped. Stated differently, the resulting transducer is
always a set.
";

const ABOUT_VERIFY: &str = "\
Performs verification on the FST to check its integrity. This works by
computing a checksum of the FST's underlying data and comparing it to an
expected checksum. If the checksums do not match, then it's likely that the
FST is corrupt in some fashion and must be re-generated.

This will also return an error if this command is called on an FST without a
checksum, such as all FSTs generated prior to 'fst 0.4'. All FSTs generated
at or after 'fst 0.4' have checksums.
";

pub fn app() -> clap::App<'static, 'static> {
    let cmd = |name, about| {
        clap::SubCommand::with_name(name)
            .author(clap::crate_authors!())
            .version(clap::crate_version!())
            .about(about)
    };
    let pos = |name| clap::Arg::with_name(name);
    let flag = |name| clap::Arg::with_name(name).long(name);

    let csv_arg_input = pos("input")
        .required(true)
        .help("The FST to extract information from.");
    let csv_arg_output = pos("output").help(
        "The CSV file to write information to. \
             When absent, print to stdout.",
    );
    let csv = cmd("csv", ABOUT_CSV)
        .subcommand(
            cmd("edges", "Emit information about edges in an FST.")
                .arg(csv_arg_input.clone())
                .arg(csv_arg_output.clone()),
        )
        .subcommand(
            cmd("nodes", "Emit information about nodes in an FST.")
                .arg(csv_arg_input.clone())
                .arg(csv_arg_output.clone()),
        );

    let dot = cmd("dot", ABOUT_DOT)
        .arg(pos("input").required(true).help("The FST to visualize."))
        .arg(pos("output").help(
            "An optional file path to write Dot output to. \
             When empty, output is written to stdout.",
        ))
        .arg(flag("state-names").help(
            " When set, states will be labeled with an arbitrary number.",
        ));

    let dupes =
        cmd("dupes", ABOUT_DUPES)
            .arg(pos("input").required(true).help("The FST to query."))
            .arg(pos("output").help(
                "An optional file path to write output to. \
                 When empty, output is written to stdout.",
            ))
            .arg(
                flag("limit")
                    .default_value("10")
                    .help("Show this many duplicate nodes."),
            )
            .arg(flag("min").default_value("20").help(
                "Only show duplicates nodes with this many reoccurrences.",
            ));

    let fuzzy = cmd("fuzzy", ABOUT_FUZZY)
        .arg(pos("input").required(true).help("The FST to query."))
        .arg(pos("query").required(true).help("The fuzzy query."))
        .arg(flag("distance").short("d").default_value("1").help(
            "All terms in the FST within this distance are shown. The \
             distance is measured in the number of character insertions, \
             deletions and substitutions required to transform the query \
             to a particular term. A \"character\" in this context refers \
             to a single Unicode codepoint.",
        ))
        .arg(
            flag("outputs")
                .short("o")
                .help("When set, output values are shown as CSV data."),
        )
        .arg(
            flag("start")
                .short("s")
                .help("Only show results greater than or equal to this."),
        )
        .arg(
            flag("end")
                .short("e")
                .help("Only show results less than or equal to this."),
        );

    let grep = cmd("grep", ABOUT_GREP)
        .arg(pos("input").required(true).help("The FST to query."))
        .arg(pos("regex").required(true).help("The regex."))
        .arg(
            flag("outputs")
                .short("o")
                .help("When set, output values are shown as CSV data."),
        )
        .arg(
            flag("start")
                .short("s")
                .help("Only show results greater than or equal to this."),
        )
        .arg(
            flag("end")
                .short("e")
                .help("Only show results less than or equal to this."),
        );

    let map = cmd("map", ABOUT_MAP)
        .arg(
            pos("input")
                .required(true)
                .multiple(true)
                .help("A file containing a key per line."),
        )
        .arg(
            pos("output")
                .required(true)
                .help("The destination file path to write the FST."),
        )
        .arg(flag("force").help(
            "Overwrites the output if the destination file already exists.",
        ))
        .arg(flag("sorted").help(
            "Set this if the input data is already lexicographically \
             sorted. This will make construction much faster. Note that \
             when this is set, most of the other flags (like --fd-limit \
             and --batch-size) are not relevant.",
        ))
        .arg(flag("max").help(
            "When building an FST from unsorted data, this merges output \
             values by taking the maximum. The default is to sum them.",
        ))
        .arg(flag("min").help(
            "When building an FST from unsorted data, this merges output \
             values by taking the minimum. The default is to sum them.",
        ))
        .arg(flag("fd-limit").default_value("15").help(
            "The maximum number of file descriptors to have open in a \
             single worker thread.",
        ))
        .arg(flag("batch-size").default_value("100000").help(
            "The number of keys to collect in each batch. N.B. This is the \
            primary factor in how much memory this process uses.",
        ))
        .arg(flag("threads").default_value("0").help(
            "The number of simultaneous workers to run. The default of 0 \
             will use the number of logical CPUs reported by your system.",
        ))
        .arg(flag("tmp-dir").takes_value(true).help(
            "A temporary directory used to store intermediate transducers. \
             This defaults to the default temporary directory reported by \
             your system.",
        ))
        .arg(flag("keep-tmp-dir").help(
            "Does not delete the temporary directory. Useful for debugging.",
        ));

    let node = cmd("node", ABOUT_NODE)
        .arg(pos("input").required(true).help("The FST to inspect."))
        .arg(
            pos("node-address")
                .required(true)
                .help("The address of the node to print."),
        );

    let range = cmd("range", ABOUT_RANGE)
        .arg(
            pos("input")
                .required(true)
                .help("The FST to run a range query against."),
        )
        .arg(
            flag("outputs")
                .short("o")
                .help("When set, output values are shown as CSV data."),
        )
        .arg(
            flag("start")
                .short("s")
                .takes_value(true)
                .help("Only show results greater than or equal to this."),
        )
        .arg(
            flag("end")
                .short("e")
                .takes_value(true)
                .help("Only show results less than or equal to this."),
        );

    let rust = cmd("rust", ABOUT_RUST)
        .arg(
            pos("input")
                .required(true)
                .help("The FST to generate Rust code for."),
        )
        .arg(
            pos("name")
                .required(true)
                .help("The name of the FST to use in the Rust source code."),
        );

    let set = cmd("set", ABOUT_SET)
        .arg(
            pos("input")
                .required(true)
                .multiple(true)
                .help("One or more files containing a key per line."),
        )
        .arg(
            pos("output")
                .required(true)
                .help("The destination file path to write the FST."),
        )
        .arg(flag("force").help(
            "Overwrites the output if the destination file already exists.",
        ))
        .arg(flag("sorted").help(
            "Set this if the input data is already lexicographically \
             sorted. This will make construction much faster. Note that \
             when this is set, most of the other flags (like --fd-limit \
             and --batch-size) are not relevant.",
        ))
        .arg(flag("fd-limit").default_value("15").help(
            "The maximum number of file descriptors to have open in a \
             single worker thread.",
        ))
        .arg(flag("batch-size").default_value("100000").help(
            "The number of keys to collect in each batch. N.B. This is the \
            primary factor in how much memory this process uses.",
        ))
        .arg(flag("threads").default_value("0").help(
            "The number of simultaneous workers to run. The default of 0 \
             will use the number of logical CPUs reported by your system.",
        ))
        .arg(flag("tmp-dir").takes_value(true).help(
            "A temporary directory used to store intermediate transducers. \
             This defaults to the default temporary directory reported by \
             your system.",
        ))
        .arg(flag("keep-tmp-dir").help(
            "Does not delete the temporary directory. Useful for debugging.",
        ));

    let union = cmd("union", ABOUT_UNION)
        .arg(
            pos("input")
                .required(true)
                .multiple(true)
                .help("One or more files containing a key per line."),
        )
        .arg(
            pos("output")
                .required(true)
                .help("The destination file path to write the FST."),
        )
        .arg(flag("force").help(
            "Overwrites the output if the destination file already exists.",
        ));

    let verify = cmd("verify", ABOUT_VERIFY).arg(
        pos("input").required(true).multiple(true).help("The FST to verify."),
    );

    clap::App::new("fst")
        .author(clap::crate_authors!())
        .version(clap::crate_version!())
        .about(ABOUT)
        .max_term_width(100)
        .setting(clap::AppSettings::UnifiedHelpMessage)
        .subcommand(csv)
        .subcommand(dot)
        .subcommand(dupes)
        .subcommand(fuzzy)
        .subcommand(grep)
        .subcommand(map)
        .subcommand(node)
        .subcommand(range)
        .subcommand(rust)
        .subcommand(set)
        .subcommand(union)
        .subcommand(verify)
}
