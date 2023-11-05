#![deny(unsafe_code)]
#![deny(
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    clippy::unwrap_used,
    clippy::expect_used
)]
#![allow(clippy::missing_errors_doc)]

mod bytecode;
mod compiler;
mod vm;

pub use compiler::{Compiler, RawDiceHandling};
pub use vm::{Value, Vm};

pub static PRELUDE: &str = r#"
let adv = |v| {
    let v1 = $ v;
    let v2 = $ v;
    if v1 > v2 then v1 else v2
};
let dis = |v| {
    let v1 = $ v;
    let v2 = $ v;
    if v1 < v2 then v1 else v2
};
let min = |xx, yy| {
    let x = $ xx;
    let y = $ yy;
    if x < y then x else y
};
let max = |xx, yy| {
    let x = $ xx;
    let y = $ yy;
    if x > y then x else y
};
"#;

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use std::rc::Rc;

    use super::*;
    use anyhow::Result;
    use once_cell::sync::OnceCell;

    static LOG: OnceCell<()> = OnceCell::new();

    fn init() {
        LOG.get_or_init(|| simple_logger::init_with_level(log::Level::Trace).unwrap());
    }

    #[test]
    fn scoping() {
        assert_value(
            r#"
                let x = 2;
                let y = 3;
                let z = 5;
                {
                    let y = 7;
                    let z = 13;
                    {
                        let z = 17;
                        x * y * z
                    }
                }
        "#,
            &Value::Number(2. * 7. * 17.),
        );
    }

    #[test]
    fn rebinding() {
        assert_value(
            r#"
                let x = 2;
                let y = x;
                let x = 3;
                x * y
        "#,
            &Value::Number(3. * 2.),
        );
    }

    #[test]
    fn calls() {
        assert_value(
            r#"
            let sub = |a, b| a - b;
            let div = |a, b| a / b;
            div(7, sub(5, 3))
        "#,
            &Value::Number(3.5),
        );
    }

    #[test]
    fn calls_as_numbers() {
        assert_value(r#"(|| 1) + (|| 2)"#, &Value::Number(3.));
    }

    #[test]
    fn if_condition() {
        assert_value(r#"if 5 > 3 then 10 else 20"#, &Value::Number(10.));
    }

    #[test]
    fn else_condition() {
        assert_value(r#"if 2 > 3 then 10 else 20"#, &Value::Number(20.));
    }

    #[test]
    fn nested_if_else() {
        assert_value(
            r#"if 2 > 3 then if 5 < 3 then 10 else 20 else if 5 > 3 then 30 else 40"#,
            &Value::Number(30.),
        );
    }

    #[test]
    fn nd() {
        assert_value(r#"5d12"#, &Value::Number(19.));
    }

    #[test]
    fn final_value_eval_no_arg_func() {
        assert_value(r#"|| d20"#, &Value::Number(9.));
    }

    #[test]
    fn dis() {
        assert_value(r#"dis(d20)"#, &Value::Number(6.));
    }

    #[test]
    fn adv() {
        assert_value(r#"adv(d20)"#, &Value::Number(9.));
    }

    #[test]
    fn mod_works() {
        assert_value(r#"10 % 3"#, &Value::Number(1.));
        assert_value(r#"10.5 % 1"#, &Value::Number(0.5));
    }

    #[test]
    fn chained_final_function_value() {
        assert_value(r#"let a = || 3; let b = || a; b"#, &Value::Number(3.));
    }

    #[test]
    fn infinite_recursion() {
        assert_err(r#"let a = a; a"#, "call stack overflow");
    }

    #[test]
    fn infinite_recursion2() {
        assert_err(r#"let a = a + a; a"#, "stack overflow");
    }

    #[test]
    fn ambiguous_identifier() {
        assert_err(
            r#"let d10 = 1; d10"#,
            "identifier d10 is ambiguous with a dice roll",
        );
    }

    #[test]
    fn ambiguous_parameter() {
        assert_err(r#"|d5| 5"#, "identifier d5 is ambiguous with a dice roll");
    }

    #[test]
    fn d_unacceptable_local() {
        assert_err(
            r#"let d = 5; d"#,
            "identifier d is ambiguous with a dice roll",
        );
    }

    #[test]
    fn list() {
        assert_value(
            r#"let x = [1, false, 2, true]; [ x[3], x[1], x[2], x[0] ]"#,
            &Value::List(Rc::new(vec![
                Value::Boolean(true),
                Value::Boolean(false),
                Value::Number(2.),
                Value::Number(1.),
            ])),
        );
    }

    #[test]
    fn list_out_of_bounds() {
        assert_err(r#"[false][1]"#, "index 1 out of bounds");
        assert_err(r#"[false][-1]"#, "invalid index");
        assert_err(r#"[][0]"#, "index 0 out of bounds");
    }

    #[test]
    fn bad_index() {
        assert_err(r#"false[0]"#, "expected list, found false");
        assert_err(r#"1[0]"#, "expected list, found 1");
        assert_err(r#"(|| false)[0]"#, "expected list, found false");
    }

    #[test]
    fn list_length() {
        assert_value(r#"[].length"#, &Value::Number(0.));
        assert_value(r#"[1].length"#, &Value::Number(1.));
        assert_value(r#"[1,1].length"#, &Value::Number(2.));
    }

    #[test]
    fn list_map() {
        assert_value(
            r#"[1, 2, 3, 4].map(|x| x * 2)"#,
            &Value::List(Rc::new(vec![
                Value::Number(2.),
                Value::Number(4.),
                Value::Number(6.),
                Value::Number(8.),
            ])),
        );
    }

    #[test]
    fn list_filter() {
        assert_value(
            r#"[1, 2, 3, 4].filter(|x| x % 2 == 0)"#,
            &Value::List(Rc::new(vec![Value::Number(2.), Value::Number(4.)])),
        );
    }

    #[test]
    fn list_random() {
        assert_value(r#"[1, 2, 3].random(0)"#, &Value::Number(1.));
        assert_value(r#"let _ = $ d2; [1, 2, 3].random(0)"#, &Value::Number(3.));
        assert_value(
            r#"let _ = $ d2 + d2; [1, 2, 3].random(0)"#,
            &Value::Number(3.),
        );
        assert_value(
            r#"let _ = $ d2 + d2 + d2; [1, 2, 3].random(0)"#,
            &Value::Number(3.),
        );
        assert_value(
            r#"let _ = $ d2 + d2 + d2 + d2; [1, 2, 3].random(0)"#,
            &Value::Number(2.),
        );
    }

    #[test]
    fn list_random_empty_list_uses_default() {
        assert_value(r#"[].random(1)"#, &Value::Number(1.));
    }

    #[test]
    fn list_random_missing_default() {
        assert_err(
            r#"[1, 2, 3].random()"#,
            "incorrect number of arguments: List.random(default_value)",
        );
    }

    #[test]
    fn list_sum() {
        assert_value(r#"[1, 2, 4, 8].sum()"#, &Value::Number(15.));
    }

    #[test]
    fn list_sum_strict() {
        assert_value(r#"let x = $ [d4, d4, d4].sum; x + x"#, &Value::Number(10.));
    }

    #[test]
    fn assignment_lazy() {
        assert_value(r#"let x = d100; x == x"#, &Value::Boolean(false));
    }

    #[test]
    fn strict_assignment_not_lazy() {
        assert_value(r#"let x = $ d100; x == x"#, &Value::Boolean(true));
    }

    #[test]
    fn tower_of_doom() {
        assert_value(
            r#"let x = || || || || || || || || || || || || || || 5; x + 1"#,
            &Value::Number(6.),
        );
        assert_value(
            r#"let x = || || || || || || || || || || || || || || false; if x then 1 else 0"#,
            &Value::Number(0.),
        );
        assert_value(
            r#"let x = || || || || || || || || || || || || || || [true]; x[0]"#,
            &Value::Boolean(true),
        );
        assert_value(
            r#"let x = || || || || || || || || || || || || || || 5; x < 3"#,
            &Value::Boolean(false),
        );
        assert_value(
            r#"let x = || || || || || || || || || || || || || || d100; let y = $ x; y == y"#,
            &Value::Boolean(true),
        );
        assert_value(
            r#"let x = || || || || || || || || || || || || || || d100; let y = x; y == y"#,
            &Value::Boolean(false),
        );
    }

    #[test]
    fn list_unknown_field() {
        assert_err(r#"[].abc"#, "unexpected field abc");
    }

    #[test]
    fn d0() {
        assert_value(r#"d0"#, &Value::Number(0.));
    }

    #[test]
    fn fib() {
        assert_value(
            r#"
                let fib = |rn| {
                    let n = $rn;
                    if n == 0 or n == 1 then n else fib(n-1) + fib(n-2)
                };
                fib(10)
            "#,
            &Value::Number(55.),
        );
    }

    #[test]
    fn boolean_operators() {
        assert_value(r#"1 == 2 or 2 == 2 and true"#, &Value::Boolean(true));
        assert_value(
            r#"true and true == true or false == true"#,
            &Value::Boolean(true),
        );
        assert_value(r#"false and (true == true)"#, &Value::Boolean(false));
        assert_value(r#"6 < 10 and 5 > 3"#, &Value::Boolean(true));
        assert_value(r#"6 < 10 and 5 <= 3"#, &Value::Boolean(false));
    }

    #[test]
    fn lazy_arguments() {
        assert_value(r#"dis(d20 + 1)"#, &Value::Number(7.));
    }

    #[test]
    fn strict_arguments() {
        assert_value(r#"dis($ d20 + 1)"#, &Value::Number(10.));
    }

    #[test]
    fn repeat() {
        assert_value(
            r#"repeat(4, d20)"#,
            &Value::List(Rc::new(
                [9., 6., 4., 5.].into_iter().map(Value::Number).collect(),
            )),
        );
    }

    #[test]
    fn d_is_d0() {
        assert_value_with(r#"d"#, &Value::Number(0.), RawDiceHandling::Zero);
        assert_value_with(r#"3d"#, &Value::Number(0.), RawDiceHandling::Zero);
    }

    #[test]
    fn d_is_d12() {
        assert_value_with(r#"d"#, &Value::Number(5.), RawDiceHandling::D12);
        assert_value_with(r#"3d"#, &Value::Number(12.), RawDiceHandling::D12);
    }

    fn assert_value(code: &str, value: &Value) {
        assert_value_with(code, value, RawDiceHandling::Zero);
    }

    fn assert_value_with(code: &str, value: &Value, raw_dice_handling: RawDiceHandling) {
        assert_eq!(&get_result(code, raw_dice_handling).unwrap(), value);
    }

    fn assert_err(code: &str, error_msg: &str) {
        assert_eq!(
            format!("{}", get_result(code, RawDiceHandling::Zero).unwrap_err()),
            error_msg
        );
    }

    fn get_result(code: &str, raw_dice_handling: RawDiceHandling) -> Result<Value> {
        init();
        let mut tmp = String::from(PRELUDE);
        tmp.push_str(code);
        let chunk = Compiler::new(raw_dice_handling).compile(tmp.as_str())?;
        let mut vm = Vm::default();
        vm.rng.seed(12345);
        vm.execute(&chunk)
    }
}
