
// (** Recognition of immediate arguments for logical integer operations.*)

// (** Valid immediate arguments are repetitions of a bit pattern [B]
//   of length [e] = 2, 4, 8, 16, 32 or 64.
//   The bit pattern [B] must be of the form [0*1*0*] or [1*0*1*]
//   but must not be all zeros or all ones. *)

// (** The following automaton recognizes [0*1*0*|1*0*1*].
// <<
//                0          1          0
//               / \        / \        / \
//               \ /        \ /        \ /
//         -0--> [B] --1--> [D] --0--> [F]
//        /
//      [A]
//        \
//         -1--> [C] --0--> [E] --1--> [G]
//               / \        / \        / \
//               \ /        \ /        \ /
//                1          0          1
// >>
// *)

// rust version

pub mod automaton {
    use ascent::ascent;

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum State {
        SA, SB, SC, SD, SE, SF, SG, SBAD
    }

    pub fn start() -> State {
        State::SA
    }

    pub fn next(state: State, b: bool) -> State {
        match (state, b) {
            (State::SA, false) => State::SB,
            (State::SA, true) => State::SC,
            (State::SB, false) => State::SB,
            (State::SB, true) => State::SD,
            (State::SC, false) => State::SBAD,
            (State::SC, true) => State::SC,
            (State::SD, false) => State::SE,
            (State::SD, true) => State::SF,
            (State::SE, false) => State::SBAD,
            (State::SE, true) => State::SE,
            (State::SF, false) => State::SG,
            (State::SF, true) => State::SBAD,
            (State::SG, false) => State::SG,
            (State::SG, true) => State::SBAD,
            (State::SBAD, _) => State::SBAD,
        } 
    }

    pub fn accepting(state: State) -> bool {
        match state {
            State::SB | State::SD | State::SF | State::SG => true,
            _ => false,
        }
    }

    pub fn run(len: usize, state: State, x: i64) -> bool {
        if len == 0 {
            accepting(state)
        } else {
            let b = (x & 1) != 0;
            run(len - 1, next(state, b), x >> 1)
        }
    }

    ascent! {
        struct AutomatonTable;

        relation start(State);
        start(State::SA);

        relation next(State, bool, State);
        next(State::SA, false, State::SB);
        next(State::SA, true, State::SC);
        next(State::SB, false, State::SB);
        next(State::SB, true, State::SD);
        next(State::SC, false, State::SBAD);
        next(State::SC, true, State::SC);
        next(State::SD, false, State::SE);
        next(State::SD, true, State::SF);
        next(State::SE, false, State::SBAD);
        next(State::SE, true, State::SE);
        next(State::SF, false, State::SG);
        next(State::SF, true, State::SBAD);
        next(State::SG, false, State::SG);
        next(State::SG, true, State::SBAD);
        next(State::SBAD, true, State::SBAD);
        next(State::SBAD, false, State::SBAD);
        relation accepting(State);
        accepting(State::SA);
        accepting(State::SB);
        accepting(State::SC);
        accepting(State::SD);
        accepting(State::SF);
        accepting(State::SG);

        index next (0, 1);
        index accepting ();
    }

    // ascent version
    ascent! {
        pub struct ImmAutomaton;
        extern database AutomatonTable auto;

        relation next(State, bool, State) in auto;
        relation accepting(State) in auto;

        // memorize the result
        relation valid_imm(i64);
        valid_imm(i) <--
            check(i), auto.accepting(s),
            do_run(0, s, x); 

        do_run(l, s1, h) <--
            do_run(l, s, x), if *l > 0,
            // let _ = println!("do_run: {:?}, {:?}, {:?}", l, s, x),
            let b = (x & 1) != 0,
            auto.next(s, b, s1),
            let h = x >> 1;

        // intermediate, need clean every time
        relation do_run(usize, State, i64);

        // need clear every time
        // input relation
        relation check(i64);
        // output relation
        relation pass();
        await do_run;
        await check;
        yield pass;

        pass() <-- check(x), valid_imm(x); 
    }

    #[test]
    fn test_imm_automaton() {
        let x = 0b11111111;
        let mut auto = AutomatonTable::default();
        auto.run();
        let mut validator = ImmAutomaton {
            check: vec![(x,)],
            do_run: vec![(8, State::SA, x)],
            ..ImmAutomaton::default()
        };
        validator.run(&auto);
        let res = validator.pass.len() == 1;
        assert!(res);
    }
}

