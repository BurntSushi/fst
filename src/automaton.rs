pub trait IntoAutomaton {
    type IntoAuto: Automaton;
    type State;

    fn into_automaton(self) -> Self::IntoAuto;
}

pub trait Automaton {
    type State: Copy;

    fn start(&self) -> Self::State;
    fn is_match(&self, state: Self::State) -> bool;
    fn accept(&self, state: Self::State, byte: u8) -> Option<Self::State>;
}

impl<T: Automaton> IntoAutomaton for T {
    type IntoAuto = T;
    type State = T::State;

    fn into_automaton(self) -> T {
        self
    }
}

pub struct AlwaysMatch;

impl Automaton for AlwaysMatch {
    type State = ();

    fn start(&self) -> () { () }
    fn is_match(&self, _: ()) -> bool { true }
    fn accept(&self, _: (), _: u8) -> Option<()> { Some(()) }
}
