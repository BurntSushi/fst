pub trait Automaton {
    type State: Copy;

    fn start(&self) -> Self::State;
    fn is_match(&self, state: Self::State) -> bool;
    fn accept(&self, state: Self::State, byte: u8) -> Option<Self::State>;
}

pub struct AlwaysMatch;

impl Automaton for AlwaysMatch {
    type State = ();

    fn start(&self) -> () { () }
    fn is_match(&self, _: ()) -> bool { true }
    fn accept(&self, _: (), _: u8) -> Option<()> { Some(()) }
}
