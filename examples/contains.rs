use fst::{automaton::Contains, IntoStreamer, Set};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let paths = vec!["a foo bar", "foo", "foo1", "foo12", "foo3", "foobar"];
    let set = Set::from_iter(paths)?;

    // Build our contains query.
    let prefix = Contains::new("foob");

    // Apply our query to the set we built.
    let stream = set.search(&prefix).into_stream();

    let matches = stream.into_strs()?;
    println!("{:?}", matches);
    Ok(())
}
