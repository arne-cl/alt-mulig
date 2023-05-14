import pytest
import sympy as sp

from recurrence import parse_recurrence, char_eqn_roots


def test_parse_recurrence_1():
    assert parse_recurrence('a(n) = 3*a(n-1) - 2*a(n-2) + a(n-3)') == [1, 3, -2, 1]

def test_parse_recurrence_2():
    assert parse_recurrence('a(n) = a(n-1) - a(n-2)') == [1, 1, -1]

def test_parse_recurrence_3():
    assert parse_recurrence('a(n) = 2*a(n-1) + a(n-2) - a(n-3)') == [1, 2, 1, -1]

def test_parse_recurrence_4():
    assert parse_recurrence('a(n) = -a(n-1) + 2*a(n-2) - 3*a(n-3)') == [1, -1, 2, -3]


def test_char_eqn_roots_1():
    eqn, roots = char_eqn_roots([1, -3, 2, -1])
    assert eqn == sp.sympify('r**3 - 3*r**2 + 2*r - 1')
    for root in roots:
        result = eqn.subs(sp.symbols('r'), root).evalf()
        print(f"Root: {root}, Result: {result}")
    assert all(abs(eqn.subs(sp.symbols('r'), root).evalf()) < 1e-10 for root in roots)

def test_char_eqn_roots_2():
    eqn, roots = char_eqn_roots([1, -1, 1])
    assert eqn == sp.sympify('r**2 - r + 1')
    for root in roots:
        result = eqn.subs(sp.symbols('r'), root).evalf()
        print(f"Root: {root}, Result: {result}")
    assert all(abs(eqn.subs(sp.symbols('r'), root).evalf()) < 1e-10 for root in roots)

def test_char_eqn_roots_3():
    eqn, roots = char_eqn_roots([1, -2, -1, 1])
    assert eqn == sp.sympify('r**3 - 2*r**2 - r + 1')
    for root in roots:
        result = eqn.subs(sp.symbols('r'), root).evalf()
        print(f"Root: {root}, Result: {result}")
    assert all(abs(eqn.subs(sp.symbols('r'), root).evalf()) < 1e-10 for root in roots)

def test_char_eqn_roots_4():
    eqn, roots = char_eqn_roots([1, 1, -2, 3])
    assert eqn == sp.sympify('r**3 + r**2 - 2*r + 3')
    for root in roots:
        result = eqn.subs(sp.symbols('r'), root).evalf()
        print(f"Root: {root}, Result: {result}")
    assert all(abs(eqn.subs(sp.symbols('r'), root).evalf()) < 1e-10 for root in roots)

def test_char_eqn_roots_5():
    eqn, roots = char_eqn_roots([1, -1, -1, 1])
    assert eqn == sp.sympify('r**3 - r**2 - r + 1')
    for root in roots:
        result = eqn.subs(sp.symbols('r'), root).evalf()
        print(f"Root: {root}, Result: {result}")
    assert all(abs(eqn.subs(sp.symbols('r'), root).evalf()) < 1e-10 for root in roots)

def test_char_eqn_roots_6():
    eqn, roots = char_eqn_roots([1, -1, 1, -1])
    assert eqn == sp.sympify('r**3 - r**2 + r - 1')
    for root in roots:
        result = eqn.subs(sp.symbols('r'), root).evalf()
        print(f"Root: {root}, Result: {result}")
    assert all(abs(eqn.subs(sp.symbols('r'), root).evalf()) < 1e-10 for root in roots)


from sympy import symbols, Eq, solve, simplify

r = symbols('r')

def test_char_eqn_roots_symbolic_1():
    # Test case 1: r**3 - 3*r**2 + 2*r - 1
    eqn, roots = char_eqn_roots([1, -3, 2, -1])
    for root in roots:
        assert simplify(eqn.subs(r, root)) == 0
    expected_roots = solve(Eq(r**3 - 3*r**2 + 2*r - 1, 0), r)
    assert set(roots) == set(expected_roots)

def test_char_eqn_roots_symbolic_2():
    # Test case 2: r**2 - r + 1
    eqn, roots = char_eqn_roots([1, -1, 1])
    for root in roots:
        assert simplify(eqn.subs(r, root)) == 0
    expected_roots = solve(Eq(r**2 - r + 1, 0), r)
    assert set(roots) == set(expected_roots)

def test_char_eqn_roots_symbolic_3():
    # Test case 3: r**3 - 2*r**2 - r + 1
    eqn, roots = char_eqn_roots([1, -2, -1, 1])
    for root in roots:
        assert simplify(eqn.subs(r, root)) == 0
    expected_roots = solve(Eq(r**3 - 2*r**2 - r + 1, 0), r)
    assert set(roots) == set(expected_roots)

def test_char_eqn_roots_symbolic_4():
    # Test case 4: r**3 + r**2 - 2*r + 3
    eqn, roots = char_eqn_roots([1, 1, -2, 3])
    for root in roots:
        assert simplify(eqn.subs(r, root)) == 0
    expected_roots = solve(Eq(r**3 + r**2 - 2*r + 3, 0), r)
    assert set(roots) == set(expected_roots)


# Run the tests
pytest.main()

