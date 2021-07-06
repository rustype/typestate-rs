// TODO document module

use super::{IntermediateAutomaton, Transition};
use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::{Debug, Display},
    hash::Hash,
};

pub trait Property {}

pub trait Validate<P: Property> {
    type Out;
    fn validate(&self, _: P) -> Self::Out;
}

type Delta<S, T> = HashMap<S, HashMap<Transition<T>, HashSet<S>>>;

pub struct GenericAutomaton<S, T>
where
    S: Hash + Eq + Debug + Clone + Display,
    T: Hash + Eq + Debug + Clone + Display,
{
    initial_states: HashSet<S>,
    final_states: HashSet<S>,
    // TODO: consider if making this pub is a decent solution
    pub states: HashSet<S>,
    delta: Delta<S, T>,
    idelta: Delta<S, T>,
}

impl<S, T> Default for GenericAutomaton<S, T>
where
    S: Hash + Eq + Debug + Clone + Display,
    T: Hash + Eq + Debug + Clone + Display,
{
    fn default() -> Self {
        Self {
            initial_states: HashSet::new(),
            final_states: HashSet::new(),
            states: HashSet::new(),
            delta: Delta::new(),
            idelta: Delta::new(),
        }
    }
}

impl<S, T> GenericAutomaton<S, T>
where
    S: Hash + Eq + Debug + Clone + Display,
    T: Hash + Eq + Debug + Clone + Display,
{
    fn add_transition(&mut self, src: S, transition: Transition<T>, dst: S) {
        // HACK: since `delta` and `idelta` are inside `self`, use the macro to as a "function"
        macro_rules! add_transition {
            ($delta:ident, $src:expr, $transition:expr, $dst:expr) => {
                let src = $src;
                let transition = $transition;
                let dst = $dst;
                if let Some(t_neighbors) = self.$delta.get_mut(&src) {
                    if let Some(n_neighbors) = t_neighbors.get_mut(&transition) {
                        n_neighbors.insert(dst);
                    } else {
                        // create the neighbor set
                        let mut v = HashSet::new();
                        v.insert(dst);
                        // connect them with the transition
                        t_neighbors.insert(transition, v);
                    }
                } else {
                    // create the neighbor set
                    let mut v = HashSet::new();
                    v.insert(dst);
                    // create the transition to neighbor map
                    let mut v_ = HashMap::new();
                    v_.insert(transition, v);
                    // connect them with the source
                    self.$delta.insert(src, v_);
                }
            };
        }

        add_transition!(delta, src.clone(), transition.clone(), dst.clone());
        add_transition!(idelta, dst, transition, src);
    }
}

impl<S, T> From<IntermediateAutomaton<S, T>> for GenericAutomaton<S, T>
where
    S: Hash + Eq + Debug + Clone + Display,
    T: Hash + Eq + Debug + Clone + Display,
{
    fn from(i: IntermediateAutomaton<S, T>) -> Self {
        let mut s = Self::default();
        // NOTE: maybe add the choices
        s.states = i.states;
        for (src, sigmas) in i.delta {
            for (sigma, dst) in sigmas {
                match (&src, dst) {
                    (None, super::Node::State(state)) => {
                        // safety: if src == None then state.state != None
                        s.initial_states.insert(state.state.unwrap());
                    }
                    (None, super::Node::Decision(_)) => {
                        unreachable!("the initial state cannot lead to a decision")
                    }
                    (Some(src), super::Node::State(state)) => match state.state {
                        None => {
                            s.final_states.insert(src.clone());
                        }
                        Some(dst) => s.add_transition(src.clone(), sigma, dst),
                    },
                    (Some(src), super::Node::Decision(states)) => {
                        states.into_iter().for_each(|state| {
                            // safety: destinations cannot point to None
                            s.add_transition(src.clone(), sigma.clone(), state.state.unwrap())
                        })
                    }
                }
            }
        }
        s
    }
}

/// Productive states property type.
pub struct ProductiveStates;

impl Property for ProductiveStates {}

impl<S, T> Validate<ProductiveStates> for GenericAutomaton<S, T>
where
    S: Hash + Eq + Debug + Clone + Display,
    T: Hash + Eq + Debug + Clone + Display,
{
    type Out = HashSet<S>;

    fn validate(&self, _: ProductiveStates) -> Self::Out {
        let mut stack: VecDeque<_> = self.final_states.iter().collect();
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
}

/// Non-productive states property type.
pub struct NonProductiveStates;

impl Property for NonProductiveStates {}

impl<S, T> Validate<NonProductiveStates> for GenericAutomaton<S, T>
where
    S: Hash + Eq + Debug + Clone + Display,
    T: Hash + Eq + Debug + Clone + Display,
{
    type Out = HashSet<S>;

    fn validate(&self, _: NonProductiveStates) -> Self::Out {
        let productive = self.validate(ProductiveStates);
        self.states.difference(&productive).cloned().collect()
    }
}

/// Useful states property type.
pub struct UsefulStates;

impl Property for UsefulStates {}

impl<S, T> Validate<UsefulStates> for GenericAutomaton<S, T>
where
    S: Hash + Eq + Debug + Clone + Display,
    T: Hash + Eq + Debug + Clone + Display,
{
    type Out = HashSet<S>;

    fn validate(&self, _: UsefulStates) -> Self::Out {
        // TODO this could benefit from some "caching" of results on productive
        let productive = self.validate(ProductiveStates);
        let mut stack: VecDeque<_> = self.initial_states.iter().collect();
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
}

/// Non-useful states property type.
pub struct NonUsefulStates;

impl Property for NonUsefulStates {}

impl<S, T> Validate<NonUsefulStates> for GenericAutomaton<S, T>
where
    S: Hash + Eq + Debug + Clone + Display,
    T: Hash + Eq + Debug + Clone + Display,
{
    type Out = HashSet<S>;

    fn validate(&self, _: NonUsefulStates) -> Self::Out {
        self.states
            .difference(&self.validate(UsefulStates))
            .cloned()
            .collect()
    }
}
