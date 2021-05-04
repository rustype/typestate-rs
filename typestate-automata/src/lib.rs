use std::{collections::{HashMap, HashSet, VecDeque}, fs::File, hash::Hash, path::Path};

#[cfg(feature = "dot")]
pub mod dot;

#[cfg(feature = "plantuml")]
pub mod plantuml;

/// Write to file operation.
pub trait TryWriteFile {
    /// Try to write `self` to the file in `path`.
    /// This operation uses the `Display` representation for its output.
    /// If successful, returns the written [File], otherwise, an [std::io::Error] is returned.
    fn try_write_file<P: AsRef<Path>>(self, path: P) -> std::io::Result<File>;
}

/// Type alias for the transition "minigraph".
type Deltas<State, Transition> = HashMap<State, HashMap<Transition, HashSet<State>>>;

/// Delta kind enumeration.
/// Distinguishes between `Delta` (forward transitions) and
/// `IDelta` (backward transitions) which are the inverse of `Delta`.
enum Delta {
    Delta,
    IDelta,
}

/// Type alias for [`DeterministicFiniteAutomata`].
pub type Dfa<State, Transition> = DeterministicFiniteAutomata<State, Transition>;

/// A representation for a deterministic finite automata.
#[derive(Debug, Clone)]
pub struct DeterministicFiniteAutomata<State, Transition>
where
    State: Eq + Hash + Clone,
    Transition: Eq + Hash + Clone,
{
    /// Deterministic finite automata states.
    pub states: HashSet<State>,
    /// Deterministic finite automata transition symbols.
    sigma: HashSet<Transition>,
    /// Deterministic finite automata initial state-indexes.
    pub initial_states: HashMap<State, HashSet<Transition>>,
    /// Deterministic finite automata final state-indexes.
    pub final_states: HashMap<State, HashSet<Transition>>,
    /// Deterministic finite automata transition functions.
    /// Map of state indexes to map of transitions to state indexes.
    delta: HashMap<State, HashMap<Transition, State>>,
    /// The inverse paths of delta.
    /// This structure helps algorithms requiring interation in the "inverse" order.
    idelta: HashMap<State, HashMap<Transition, State>>,
}

impl<State, Transition> DeterministicFiniteAutomata<State, Transition>
where
    State: Eq + Hash + Clone,
    Transition: Eq + Hash + Clone,
{
    /// Construct a new deterministic finite automata.
    #[must_use]
    pub fn new() -> Self {
        Self {
            states: HashSet::new(),
            sigma: HashSet::new(),
            initial_states: HashMap::new(),
            final_states: HashMap::new(),
            delta: HashMap::new(),
            idelta: HashMap::new(),
        }
    }

    /// Add a state to the automata.
    pub fn add_state(&mut self, state: State) {
        self.states.insert(state);
    }

    /// Add an initial state to the automata.
    pub fn add_initial(&mut self, state: State, symbol: Transition) {
        self.states.insert(state.clone());
        if let Some(symbols) = self.initial_states.get_mut(&state) {
            symbols.insert(symbol);
        } else {
            let mut symbols = HashSet::new();
            symbols.insert(symbol);
            self.initial_states.insert(state, symbols);
        }
    }

    /// Add a final state to the automata.
    pub fn add_final(&mut self, state: State, symbol: Transition) {
        self.states.insert(state.clone());
        if let Some(symbols) = self.final_states.get_mut(&state) {
            symbols.insert(symbol);
        } else {
            let mut symbols = HashSet::new();
            symbols.insert(symbol);
            self.final_states.insert(state, symbols);
        }
    }

    /// Add a new symbol to the automata alphabet.
    pub fn add_sigma(&mut self, sigma: Transition) {
        self.sigma.insert(sigma);
    }

    fn add_delta(&mut self, source: State, symbol: Transition, destination: State, delta: &Delta) {
        let delta = match delta {
            Delta::Delta => &mut self.delta,
            Delta::IDelta => &mut self.idelta,
        };
        if let Some(transitions) = delta.get_mut(&source) {
            if transitions.get_mut(&symbol).is_some() {
                // TODO duplicate: return error or replace?
            } else {
                transitions.insert(symbol, destination);
            }
        } else {
            let mut transitions = HashMap::new();
            transitions.insert(symbol, destination);
            delta.insert(source, transitions);
        }
    }

    // Add a transition from `source` to `destination`, consuming `symbol`.
    pub fn add_transition(&mut self, source: State, symbol: Transition, destination: State) {
        // TODO check for state existence or add regardless
        self.add_sigma(symbol.clone());
        self.add_delta(
            source.clone(),
            symbol.clone(),
            destination.clone(),
            &Delta::Delta,
        );
        self.add_delta(destination, symbol, source, &Delta::IDelta);
    }

    /// Compute the automata productive states.
    #[must_use]
    pub fn productive_states(&self) -> HashSet<State> {
        let mut stack: VecDeque<_> = self.final_states.keys().collect();
        // productive == visited
        let mut productive = HashSet::new();
        while let Some(state) = stack.pop_back() {
            if productive.insert(state.clone()) {
                if let Some(states) = self
                    .idelta
                    .get(state)
                    .map(std::collections::HashMap::values)
                {
                    stack.extend(states)
                }
            }
        }
        productive
    }

    /// Compute the automata non-productive states.
    ///
    /// This is done by calling [`DeterministicFiniteAutomata::productive_states`] and performing a diference against the full state set.
    #[must_use]
    pub fn non_productive_states(&self) -> HashSet<State> {
        // TODO check if the clone should be here or if we can return `HashSet<&State>`
        self.states
            .difference(&self.productive_states())
            .cloned()
            .collect()
    }

    /// Compute the automata useful states.
    #[must_use]
    pub fn useful_states(&self) -> HashSet<State> {
        // TODO this could benefit from some "caching" of results on productive
        let productive = self.productive_states();
        let mut stack: VecDeque<_> = self.initial_states.keys().collect();
        // productive == visited
        let mut reachable = HashSet::new();
        while let Some(state) = stack.pop_back() {
            if reachable.insert(state.clone()) {
                if let Some(states) = self.delta.get(state).map(std::collections::HashMap::values) {
                    stack.extend(states)
                }
            }
        }
        productive.intersection(&reachable).cloned().collect()
    }

    /// Compute the automata non-useful states.
    ///
    /// This is done by calling [`DeterministicFiniteAutomata::useful_states`] and performing a diference against the full state set.
    #[must_use]
    pub fn non_useful_states(&self) -> HashSet<State> {
        // TODO check if the clone should be here or if we can return `HashSet<&State>`
        self.states
            .difference(&self.useful_states())
            .cloned()
            .collect()
    }
}

/// Implementation of the [`Default`] trait for a [`DeterministicFiniteAutomata`].
impl<State, Transition> Default for DeterministicFiniteAutomata<State, Transition>
where
    State: Eq + Hash + Clone,
    Transition: Eq + Hash + Clone,
{
    /// This function is equivalent to [`DeterministicFiniteAutomata::new`].
    fn default() -> Self {
        Self::new()
    }
}

/// Tests for [DeterministicFiniteAutomata].
#[cfg(test)]
mod dfa_tests {
    use super::test_traits::*;
    use super::*;

    fn setup_automata() -> Dfa<i32, usize> {
        let mut dfa = Dfa::new();
        let state_list = [1, 2, 3, 4, 5, 6, 7];
        for &state in state_list.iter() {
            dfa.add_state(state);
        }
        dfa.add_initial(1, 0);
        dfa.add_final(7, 0);
        let transition_list = [
            (1, 2),
            (1, 3),
            (2, 6),
            (3, 4),
            (3, 5),
            (3, 6),
            (4, 5),
            (5, 7),
            (6, 7),
        ];
        for (idx, &(src, dst)) in transition_list.iter().enumerate() {
            dfa.add_transition(src, idx + 1, dst);
        }
        dfa
    }

    fn setup_automata_loop() -> Dfa<i32, ()> {
        let mut dfa = Dfa::new();
        dfa.add_initial(1, ());
        dfa.add_final(2, ());
        dfa.add_transition(1, (), 2);
        dfa.add_transition(2, (), 1);
        dfa
    }

    #[test]
    fn test_add_state() {
        let dfa = setup_automata();
        let expected_states = [1, 2, 3, 4, 5, 6, 7].into_hash_set();
        let result_states = dfa.states;
        assert_eq!(expected_states, result_states);
    }

    #[test]
    fn test_add_initial_state() {
        let dfa = setup_automata();
        let expected_states = [1].into_hash_set();
        let result_states: HashSet<i32> = dfa.initial_states.keys().map(|i| *i).collect();
        assert_eq!(expected_states, result_states);
    }

    #[test]
    fn test_add_final_state() {
        let dfa = setup_automata();
        let expected_states = [7].into_hash_set();
        let result_states: HashSet<i32> = dfa.final_states.keys().map(|i| *i).collect();
        assert_eq!(expected_states, result_states);
    }

    #[test]
    fn test_add_transition() {
        let dfa = setup_automata();
        let expected_deltas = [
            (1, 2),
            (1, 3),
            (2, 6),
            (3, 4),
            (3, 5),
            (3, 6),
            (4, 5),
            (5, 7),
            (6, 7),
        ]
        .iter()
        .map(|t| t.to_owned())
        .collect::<HashSet<_>>();
        let expected_ideltas = expected_deltas
            .iter()
            .map(|(fst, snd)| (*snd, *fst))
            .map(|t| t.to_owned())
            .collect::<HashSet<_>>();
        let mut result_deltas = HashSet::new();
        for (src, transitions) in dfa.delta {
            for &dst in transitions.values() {
                result_deltas.insert((src, dst));
            }
        }
        let mut result_ideltas = HashSet::new();
        for (src, transitions) in dfa.idelta {
            for &dst in transitions.values() {
                result_ideltas.insert((src, dst));
            }
        }
        assert_eq!(expected_deltas, result_deltas);
        assert_eq!(expected_ideltas, result_ideltas);
    }

    #[test]
    fn test_productive() {
        let dfa = setup_automata();
        let result = dfa.productive_states();
        let expected = [1, 2, 3, 4, 5, 6, 7]
            .iter()
            .map(|i| i.to_owned())
            .collect::<HashSet<i32>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_productive_loop() {
        let dfa = setup_automata_loop();
        let result = dfa.productive_states();
        let expected = [1, 2]
            .iter()
            .map(|i| i.to_owned())
            .collect::<HashSet<i32>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_useful() {
        let dfa = setup_automata();
        let result = dfa.useful_states();
        let expected = [1, 2, 3, 4, 5, 6, 7]
            .iter()
            .map(|i| i.to_owned())
            .collect::<HashSet<i32>>();
        assert_eq!(expected, result);
    }
}

/// Type alias for [`NonDeterministicFiniteAutomata`].
pub type Nfa<State, Transition> = NonDeterministicFiniteAutomata<State, Transition>;

/// A representation for non-deterministic finite automata.
#[derive(Debug, Clone)]
pub struct NonDeterministicFiniteAutomata<State, Transition>
where
    State: Eq + Hash + Clone,
    Transition: Eq + Hash + Clone,
{
    /// Deterministic finite automata states.
    pub states: HashSet<State>,
    /// Deterministic finite automata transition symbols.
    sigma: HashSet<Transition>,
    /// Deterministic finite automata initial state-indexes.
    pub initial_states: HashMap<State, HashSet<Transition>>,
    /// Deterministic finite automata final state-indexes.
    pub final_states: HashMap<State, HashSet<Transition>>,
    /// Deterministic finite automata transition functions.
    /// Map of state indexes to map of transitions to state indexes.
    delta: Deltas<State, Transition>,
    /// The inverse paths of delta.
    /// This structure helps algorithms requiring interation in the "inverse" order.
    idelta: Deltas<State, Transition>,
}

impl<State, Transition> NonDeterministicFiniteAutomata<State, Transition>
where
    State: Eq + Hash + Clone,
    Transition: Eq + Hash + Clone,
{
    /// Construct a new non-deterministic finite automata.
    #[must_use]
    pub fn new() -> Self {
        Self {
            states: HashSet::new(),
            sigma: HashSet::new(),
            initial_states: HashMap::new(),
            final_states: HashMap::new(),
            delta: HashMap::new(),
            idelta: HashMap::new(),
        }
    }

    /// Add a state to the automata.
    pub fn add_state(&mut self, state: State) {
        self.states.insert(state);
    }

    /// Add an initial state to the automata.
    pub fn add_initial(&mut self, state: State, symbol: Transition) {
        self.states.insert(state.clone());
        if let Some(symbols) = self.initial_states.get_mut(&state) {
            symbols.insert(symbol);
        } else {
            let mut symbols = HashSet::new();
            symbols.insert(symbol);
            self.initial_states.insert(state, symbols);
        }
    }

    /// Add a final state to the automata.
    pub fn add_final(&mut self, state: State, symbol: Transition) {
        self.states.insert(state.clone());
        if let Some(symbols) = self.final_states.get_mut(&state) {
            symbols.insert(symbol);
        } else {
            let mut symbols = HashSet::new();
            symbols.insert(symbol);
            self.final_states.insert(state, symbols);
        }
    }

    /// Add a new symbol to the automata alphabet.
    pub fn add_sigma(&mut self, sigma: Transition) {
        self.sigma.insert(sigma);
    }

    fn add_delta(&mut self, source: State, symbol: Transition, destination: State, delta: &Delta) {
        let delta = match delta {
            Delta::Delta => &mut self.delta,
            Delta::IDelta => &mut self.idelta,
        };
        if let Some(transitions) = delta.get_mut(&source) {
            if let Some(destinations) = transitions.get_mut(&symbol) {
                destinations.insert(destination);
            } else {
                let mut destinations = HashSet::new();
                destinations.insert(destination);
                transitions.insert(symbol, destinations);
            }
        } else {
            let mut transitions = HashMap::new();
            let mut destinations = HashSet::new();
            destinations.insert(destination);
            transitions.insert(symbol, destinations);
            delta.insert(source, transitions);
        }
    }

    // Add a transition from `source` to `destination`, consuming `symbol`.
    pub fn add_transition(&mut self, source: State, symbol: Transition, destination: State) {
        // TODO check for state existence or add regardless
        self.add_sigma(symbol.clone());
        self.add_delta(
            source.clone(),
            symbol.clone(),
            destination.clone(),
            &Delta::Delta,
        );
        self.add_delta(destination, symbol, source, &Delta::IDelta);
    }

    // Add a transition from `source` to `destinations`, consuming `symbol`.
    pub fn add_non_deterministic_transitions(
        &mut self,
        source: &State,
        symbol: &Transition,
        destinations: impl Iterator<Item = State>,
    ) {
        // TODO check for state existence or add regardless
        self.add_sigma(symbol.clone());
        for destination in destinations {
            self.add_delta(
                source.clone(),
                symbol.clone(),
                destination.clone(),
                &Delta::Delta,
            );
            self.add_delta(
                destination.clone(),
                symbol.clone(),
                source.clone(),
                &Delta::IDelta,
            );
        }
    }

    /// Compute the automata productive states.
    #[must_use]
    pub fn productive_states(&self) -> HashSet<State> {
        let mut stack: VecDeque<_> = self.final_states.keys().collect();
        // productive == visited
        let mut productive = HashSet::new();
        while let Some(state) = stack.pop_back() {
            if productive.insert(state.clone()) {
                if let Some(states) = self.idelta.get(state).map(|transitions| {
                    transitions
                        .values()
                        .flat_map(std::collections::HashSet::iter)
                }) {
                    stack.extend(states)
                }
            }
        }
        productive
    }

    /// Compute the automata non-productive states.
    ///
    /// This is done by calling [`NonDeterministicFiniteAutomata::productive_states`] and performing a diference against the full state set.
    #[must_use]
    pub fn non_productive_states(&self) -> HashSet<State> {
        // TODO check if the clone should be here or if we can return `HashSet<&State>`
        self.states
            .difference(&self.productive_states())
            .cloned()
            .collect()
    }

    /// Compute the automata useful states.
    #[must_use]
    pub fn useful_states(&self) -> HashSet<State> {
        // TODO this could benefit from some "caching" of results on productive
        let productive = self.productive_states();
        let mut stack: VecDeque<_> = self.initial_states.keys().collect();
        // productive == visited
        let mut reachable = HashSet::new();
        while let Some(state) = stack.pop_back() {
            if reachable.insert(state.clone()) {
                if let Some(states) = self.delta.get(state).map(|transitions| {
                    transitions
                        .values()
                        .flat_map(std::collections::HashSet::iter)
                }) {
                    stack.extend(states)
                }
            }
        }
        productive.intersection(&reachable).cloned().collect()
    }

    /// Compute the automata non-useful states.
    ///
    /// This is done by calling [`NonDeterministicFiniteAutomata::useful_states`] and performing a diference against the full state set.
    #[must_use]
    pub fn non_useful_states(&self) -> HashSet<State> {
        // TODO check if the clone should be here or if we can return `HashSet<&State>`
        self.states
            .difference(&self.useful_states())
            .cloned()
            .collect()
    }
}

/// Implementation of the [`Default`] trait for a [`NonDeterministicFiniteAutomata`].
impl<State, Transition> Default for NonDeterministicFiniteAutomata<State, Transition>
where
    State: Eq + Hash + Clone,
    Transition: Eq + Hash + Clone,
{
    /// This function is equivalent to [`NonDeterministicFiniteAutomata::new`].
    fn default() -> Self {
        Self::new()
    }
}

/// Tests for [`NonDeterministicFiniteAutomata`].
#[cfg(test)]
mod nfa_tests {
    use super::test_traits::*;
    use super::*;

    fn setup_automata() -> Nfa<i32, ()> {
        let mut nfa = Nfa::new();
        let state_list = [1, 2, 3, 4, 5, 6, 7];
        for &state in state_list.iter() {
            nfa.add_state(state);
        }
        nfa.add_initial(1, ());
        nfa.add_final(7, ());
        let transition_list = [
            (1, 2),
            (1, 3),
            (2, 6),
            (3, 4),
            (3, 5),
            (3, 6),
            (4, 5),
            (5, 7),
            (6, 7),
        ];
        for &(src, dst) in transition_list.iter() {
            nfa.add_transition(src, (), dst);
        }
        nfa
    }

    fn setup_automata_loop() -> Nfa<i32, ()> {
        let mut nfa = Nfa::new();
        nfa.add_initial(1, ());
        nfa.add_final(2, ());
        nfa.add_transition(1, (), 2);
        nfa.add_transition(2, (), 1);
        nfa
    }

    #[test]
    fn test_add_state() {
        let nfa = setup_automata();
        let expected_states = [1, 2, 3, 4, 5, 6, 7].into_hash_set();
        let result_states = nfa.states;
        assert_eq!(expected_states, result_states);
    }

    #[test]
    fn test_add_initial_state() {
        let nfa = setup_automata();
        let expected_states = [1].into_hash_set();
        let result_states: HashSet<i32> = nfa.initial_states.keys().map(|i| *i).collect();
        assert_eq!(expected_states, result_states);
    }

    #[test]
    fn test_add_final_state() {
        let nfa = setup_automata();
        let expected_states = [7].into_hash_set();
        let result_states: HashSet<i32> = nfa.final_states.keys().map(|i| *i).collect();
        assert_eq!(expected_states, result_states);
    }

    #[test]
    fn test_add_transition() {
        let nfa = setup_automata();
        let expected_deltas = [
            (1, 2),
            (1, 3),
            (2, 6),
            (3, 4),
            (3, 5),
            (3, 6),
            (4, 5),
            (5, 7),
            (6, 7),
        ]
        .iter()
        .map(|t| t.to_owned())
        .collect::<HashSet<_>>();
        let expected_ideltas = expected_deltas
            .iter()
            .map(|(fst, snd)| (*snd, *fst))
            .map(|t| t.to_owned())
            .collect::<HashSet<_>>();
        let mut result_deltas = HashSet::new();
        for (src, transitions) in nfa.delta {
            for destinations in transitions.values() {
                for &dst in destinations {
                    result_deltas.insert((src, dst));
                }
            }
        }
        let mut result_ideltas = HashSet::new();
        for (src, transitions) in nfa.idelta {
            for destinations in transitions.values() {
                for &dst in destinations {
                    result_ideltas.insert((src, dst));
                }
            }
        }
        assert_eq!(expected_deltas, result_deltas);
        assert_eq!(expected_ideltas, result_ideltas);
    }

    #[test]
    fn test_add_non_deterministic_transition() {
        let mut nfa = Nfa::new();
        [1, 2, 3, 4, 5, 6, 7]
            .iter()
            .map(|i| *i)
            .for_each(|i| nfa.add_state(i));
        nfa.add_initial(1, ());
        nfa.add_final(7, ());
        let non_deterministic_transitions = vec![
            (1, vec![2, 3]),
            (2, vec![6]),
            (3, vec![4, 5, 6]),
            (4, vec![5]),
            (5, vec![7]),
            (6, vec![7]),
        ];
        for (source, destinations) in non_deterministic_transitions {
            nfa.add_non_deterministic_transitions(&source, &(), destinations.into_iter());
        }
        let expected_deltas = [
            (1, 2),
            (1, 3),
            (2, 6),
            (3, 4),
            (3, 5),
            (3, 6),
            (4, 5),
            (5, 7),
            (6, 7),
        ]
        .iter()
        .map(|t| t.to_owned())
        .collect::<HashSet<_>>();
        let expected_ideltas = expected_deltas
            .iter()
            .map(|(fst, snd)| (*snd, *fst))
            .map(|t| t.to_owned())
            .collect::<HashSet<_>>();
        let mut result_deltas = HashSet::new();
        for (src, transitions) in nfa.delta {
            for destinations in transitions.values() {
                for &dst in destinations {
                    result_deltas.insert((src, dst));
                }
            }
        }
        let mut result_ideltas = HashSet::new();
        for (src, transitions) in nfa.idelta {
            for destinations in transitions.values() {
                for &dst in destinations {
                    result_ideltas.insert((src, dst));
                }
            }
        }
        assert_eq!(expected_deltas, result_deltas);
        assert_eq!(expected_ideltas, result_ideltas);
    }

    #[test]
    fn test_productive() {
        let nfa = setup_automata();
        let result = nfa.productive_states();
        let expected = [1, 2, 3, 4, 5, 6, 7]
            .iter()
            .map(|i| i.to_owned())
            .collect::<HashSet<i32>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_productive_loop() {
        let nfa = setup_automata_loop();
        let result = nfa.productive_states();
        let expected = [1, 2]
            .iter()
            .map(|i| i.to_owned())
            .collect::<HashSet<i32>>();
        assert_eq!(expected, result);
    }

    #[test]
    fn test_useful() {
        let nfa = setup_automata();
        let result = nfa.useful_states();
        let expected = [1, 2, 3, 4, 5, 6, 7]
            .iter()
            .map(|i| i.to_owned())
            .collect::<HashSet<i32>>();
        assert_eq!(expected, result);
    }
}

/// Module containing useful traits for tests.
#[cfg(test)] // should only be available for tests
mod test_traits {
    use std::{collections::HashSet, hash::Hash, rc::Rc};

    /// A conversion to HashSet<T> trait.
    pub(crate) trait IntoHashSet<T>
    where
        T: Eq + Hash,
    {
        fn into_hash_set(self) -> HashSet<T>;
    }

    /// Implementation of [IntoHashSet<T>] for a slice of `i32` integers.
    impl<const N: usize> IntoHashSet<i32> for [i32; N] {
        fn into_hash_set(self) -> HashSet<i32> {
            self.iter().map(|t| t.to_owned()).collect()
        }
    }

    /// Implementation of [IntoHashSet<T>] for an `HashSet<Rc<i32>>`.
    impl IntoHashSet<i32> for HashSet<Rc<i32>> {
        fn into_hash_set(self) -> HashSet<i32> {
            self.iter().map(|i| *i.to_owned()).collect()
        }
    }
}
