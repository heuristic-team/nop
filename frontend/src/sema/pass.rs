use super::Res;

/// Trait for all semantic analyser passes, providing an interface to run and chain them.
pub trait Pass: Sized {
    type Input;
    type Output;

    fn run(&mut self, input: Self::Input) -> Res<Self::Output>;

    /// Create a pass that combines `self` and `other`. The second pass is ran iff the first pass returned `Ok` or `Problematic`. In the latter case all diagnostics from `self` are preserved.
    fn and_then<O>(self, other: O) -> impl Pass<Input = Self::Input, Output = O::Output>
    where
        O: Pass<Input = Self::Output>,
    {
        PassComposition::new(self, other)
    }
}

struct PassComposition<P1, P2>
where
    P1: Pass,
    P2: Pass<Input = P1::Output>,
{
    first: P1,
    second: P2,
}

impl<P1, P2> PassComposition<P1, P2>
where
    P1: Pass,
    P2: Pass<Input = P1::Output>,
{
    pub fn new(first: P1, second: P2) -> Self {
        Self { first, second }
    }
}

impl<P1, P2> Pass for PassComposition<P1, P2>
where
    P1: Pass,
    P2: Pass<Input = P1::Output>,
{
    type Input = P1::Input;
    type Output = P2::Output;

    fn run(&mut self, input: Self::Input) -> Res<Self::Output> {
        self.first.run(input).and_then(|res| self.second.run(res))
    }
}
