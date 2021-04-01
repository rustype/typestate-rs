use std::{
    collections::{HashMap, HashSet, VecDeque},
    hash::Hash,
};

/// Type alias for the transition "minigraph".
type Deltas<State, Transition> = HashMap<State, HashMap<Transition, HashSet<State>>>;

/// Delta kind enumeration.
/// Distinguishes between `Delta` (forward transitions) and
/// `IDelta` (backward transitions) which are the inverse of `Delta`.
enum Delta {
    Delta,
    IDelta,
}

/// Type alias for [FiniteAutomata].
pub type FA<State, Transition> = FiniteAutomata<State, Transition>;

/// A representation for finite automata.
pub struct FiniteAutomata<State, Transition>
where
    State: Eq + Hash + Clone,
    Transition: Eq + Hash + Clone,
{
    /// Finite automata states.
    pub states: HashSet<State>,
    /// Finite automata transition symbols.
    sigma: HashSet<Transition>,
    /// Finite automata initial state-indexes.
    pub initial_states: HashSet<State>,
    /// Finite automata final state-indexes.
    pub final_states: HashSet<State>,
    /// Finite automata transition functions.
    /// Map of state indexes to map of transitions to state indexes.
    delta: Deltas<State, Transition>,
    /// The inverse paths of delta.
    /// This structure helps algorithms requiring interation in the "inverse" order.
    idelta: Deltas<State, Transition>,
}

impl<State, Transition> FiniteAutomata<State, Transition>
where
    State: Eq + Hash + Clone,
    Transition: Eq + Hash + Clone,
{
    /// Construct a new finite automata.
    pub fn new() -> Self {
        Self {
            states: HashSet::new(),
            sigma: HashSet::new(),
            initial_states: HashSet::new(),
            final_states: HashSet::new(),
            delta: HashMap::new(),
            idelta: HashMap::new(),
        }
    }

    /// Add a state to the automata.
    pub fn add_state(&mut self, state: State) {
        self.states.insert(state);
    }

    /// Add an initial state to the automata.
    pub fn add_initial(&mut self, state: State) {
        self.states.insert(state.clone());
        self.initial_states.insert(state);
    }

    /// Add a final state to the automata.
    pub fn add_final(&mut self, state: State) {
        self.states.insert(state.clone());
        self.final_states.insert(state);
    }

    /// Add a new symbol to the automata alphabet.
    pub fn add_sigma(&mut self, sigma: Transition) {
        self.sigma.insert(sigma);
    }

    fn add_delta(&mut self, source: State, symbol: Transition, destination: State, delta: Delta) {
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
            Delta::Delta,
        );
        self.add_delta(source, symbol, destination, Delta::IDelta);
    }

    /// Compute the automata productive states.
    pub fn productive_states(&self) -> HashSet<State> {
        let mut stack: VecDeque<_> = self.final_states.iter().collect();
        // productive == visited
        let mut productive = HashSet::new();
        while let Some(state) = stack.pop_back() {
            if productive.insert(state.clone()) {
                if let Some(states) = self
                    .idelta
                    .get(state)
                    .map(|transitions| transitions.values().flat_map(|states| states.iter()))
                {
                    stack.extend(states)
                }
            }
        }
        productive
    }

    /// Compute the automata useful states.
    pub fn useful_states(&self) -> HashSet<State> {
        // TODO this could benefit from some "caching" of results on productive
        let productive = self.productive_states();
        let mut stack: VecDeque<_> = self.initial_states.iter().collect();
        // productive == visited
        let mut reachable = HashSet::new();
        while let Some(state) = stack.pop_back() {
            if reachable.insert(state.clone()) {
                if let Some(states) = self
                    .delta
                    .get(state)
                    .map(|transitions| transitions.values().flat_map(|states| states.iter()))
                {
                    stack.extend(states)
                }
            }
        }
        productive
            .intersection(&reachable)
            .map(|s| s.to_owned())
            .collect()
    }
}

/// Implementation of the [Default] trait for a [FiniteAutomata].
impl<State, Transition> Default for FiniteAutomata<State, Transition>
where
    State: Eq + Hash + Clone,
    Transition: Eq + Hash + Clone,
{
    /// This function is equivalent to [FiniteAutomata::new].
    fn default() -> Self {
        Self::new()
    }
}

/// Type alias for [DeterministicFiniteAutomata].
pub type DFA<State, Transition> = DeterministicFiniteAutomata<State, Transition>;

/// A representation for a deterministic finite automata.
pub struct DeterministicFiniteAutomata<State, Transition>
where
    State: Eq + Hash + Clone,
    Transition: Eq + Hash + Clone,
{
    pub automata: FiniteAutomata<State, Transition>,
}

impl<State, Transition> DeterministicFiniteAutomata<State, Transition>
where
    State: Eq + Hash + Clone,
    Transition: Eq + Hash + Clone,
{
    /// Construct a new deterministic finite automata.
    pub fn new() -> Self {
        Self {
            automata: FiniteAutomata::new(),
        }
    }

    /// Add a state to the automata.
    pub fn add_state(&mut self, state: State) {
        self.automata.add_state(state);
    }

    /// Add an initial state to the automata.
    pub fn add_initial(&mut self, state: State) {
        self.automata.add_initial(state);
    }

    /// Add a final state to the automata.
    pub fn add_final(&mut self, state: State) {
        self.automata.add_final(state);
    }

    /// Add a new symbol to the automata alphabet.
    pub fn add_sigma(&mut self, sigma: Transition) {
        self.automata.add_sigma(sigma);
    }

    // Add a transition from `source` to `destination`, consuming `symbol`.
    pub fn add_transition(&mut self, source: State, symbol: Transition, destination: State) {
        self.automata.add_transition(source, symbol, destination);
    }

    /// Compute the automata productive states.
    pub fn productive_states(&self) -> HashSet<State> {
        self.automata.productive_states()
    }

    /// Compute the automata useful states.
    pub fn useful_states(&self) -> HashSet<State> {
        self.automata.useful_states()
    }
}

/// Implementation of the [Default] trait for a [DeterministicFiniteAutomata].
/// This function is equivalent to [DeterministicFiniteAutomata::new].
impl<State, Transition> Default for DeterministicFiniteAutomata<State, Transition>
where
    State: Eq + Hash + Clone,
    Transition: Eq + Hash + Clone,
{
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod dfa_tests {
    use super::*;

    fn setup_automata() -> DFA<i32, ()> {
        let mut dfa = DFA::new();
        dfa.add_state(1);
        dfa.add_state(2);
        dfa.add_state(3);
        dfa.add_state(4);
        dfa.add_state(5);
        dfa.add_state(6);
        dfa.add_state(7);
        dfa.add_initial(1);
        dfa.add_final(7);
        dfa.add_transition(1, (), 2);
        dfa.add_transition(1, (), 3);
        dfa.add_transition(2, (), 6);
        dfa.add_transition(3, (), 4);
        dfa.add_transition(3, (), 5);
        dfa.add_transition(3, (), 6);
        dfa.add_transition(4, (), 5);
        dfa.add_transition(5, (), 7);
        dfa.add_transition(6, (), 7);
        dfa
    }

    fn setup_automata_loop() -> DFA<i32, ()> {
        let mut dfa = DFA::new();
        dfa.add_initial(1);
        dfa.add_final(2);
        dfa.add_transition(1, (), 2);
        dfa.add_transition(2, (), 1);
        dfa
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

/// Type alias for [NonDeterministicFiniteAutomata].
pub type NFA<State, Transition> = NonDeterministicFiniteAutomata<State, Transition>;

/// A representation for non-deterministic finite automata.
pub struct NonDeterministicFiniteAutomata<State, Transition>
where
    State: Eq + Hash + Clone,
    Transition: Eq + Hash + Clone,
{
    pub automata: FiniteAutomata<State, Transition>,
}

impl<State, Transition> NonDeterministicFiniteAutomata<State, Transition>
where
    State: Eq + Hash + Clone,
    Transition: Eq + Hash + Clone,
{
    /// Construct a new deterministic finite automata.
    pub fn new() -> Self {
        Self {
            automata: FiniteAutomata::new(),
        }
    }

    /// Add a state to the automata.
    pub fn add_state(&mut self, state: State) {
        self.automata.add_state(state);
    }

    /// Add an initial state to the automata.
    pub fn add_initial(&mut self, state: State) {
        self.automata.add_initial(state);
    }

    /// Add a final state to the automata.
    pub fn add_final(&mut self, state: State) {
        self.automata.add_final(state);
    }

    /// Add a new symbol to the automata alphabet.
    pub fn add_sigma(&mut self, sigma: Transition) {
        self.automata.add_sigma(sigma);
    }

    // Add a transition from `source` to `destination`, consuming `symbol`.
    pub fn add_transition(&mut self, source: State, symbol: Transition, destination: State) {
        self.automata.add_transition(source, symbol, destination);
    }

    // Add a transition from `source` to `destinations`, consuming `symbol`.
    pub fn add_non_deterministic_transitions(
        &mut self,
        source: State,
        symbol: Transition,
        destinations: impl Iterator<Item = State>,
    ) {
        // TODO check for state existence or add regardless
        self.automata.add_sigma(symbol.clone());
        for destination in destinations {
            self.automata.add_delta(
                source.clone(),
                symbol.clone(),
                destination.clone(),
                Delta::Delta,
            );
            self.automata.add_delta(
                source.clone(),
                symbol.clone(),
                destination.clone(),
                Delta::IDelta,
            );
        }
    }

    /// Compute the automata productive states.
    pub fn productive_states(&self) -> HashSet<State> {
        self.automata.productive_states()
    }

    /// Compute the automata useful states.
    pub fn useful_states(&self) -> HashSet<State> {
        self.automata.useful_states()
    }
}

/// Implementation of the [Default] trait for a [NonDeterministicFiniteAutomata].
impl<State, Transition> Default for NonDeterministicFiniteAutomata<State, Transition>
where
    State: Eq + Hash + Clone,
    Transition: Eq + Hash + Clone,
{
    /// This function is equivalent to [NonDeterministicFiniteAutomata::new].
    fn default() -> Self {
        Self::new()
    }
}
