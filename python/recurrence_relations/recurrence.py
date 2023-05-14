import sympy as sp

# ~ def char_eqn_roots(coefficients):
    # ~ r = sp.symbols('r')
    # ~ n = len(coefficients)
    # ~ eqn = sum(coefficients[i]*r**(n - i - 1) for i in range(n))
    # ~ roots = sp.solve(eqn, r)
    # ~ roots = [root.simplify() for root in roots]
    # ~ return eqn, roots

def char_eqn_roots(coeffs):
    r = sp.symbols('r')
    eqn = sum(coeff*r**i for i, coeff in enumerate(reversed(coeffs)))
    roots_dict = sp.roots(eqn, r)
    roots = [root for root, multiplicity in roots_dict.items() for _ in range(multiplicity)]
    return eqn, roots

# Usage example:
# ~ coefficients = [1, -1, -2]  # coefficients for a(n) = a(n-1) + 2*a(n-2)
# ~ equation, roots = char_eqn_roots(coefficients)
# ~ print(f"Characteristic equation: {equation}")
# ~ print(f"Roots: {roots}")


import re

def parse_recurrence(relation):
    relation = relation.replace(" ", "")
    relation = relation.split("=")[1]
    terms = re.findall(r'([+-]?\d*)\*?a\(n-\d+\)', relation)
    coefficients = []
    for term in terms:
        if term == '':
            coefficients.append(1)
        elif term == '-':
            coefficients.append(-1)
        elif term[0] == '+':
            if term[1:] == '':
                coefficients.append(1)
            else:
                coefficients.append(int(term[1:]))
        else:
            coefficients.append(int(term))
    coefficients.insert(0, 1)
    return coefficients


# Test the function
# ~ relation = 'a(n) = 3*a(n-1) - 2*a(n-2) + a(n-3)'
# ~ coefficients = parse_recurrence(relation)
# ~ print(coefficients)  # Output: [1, 3, -2, 1]



