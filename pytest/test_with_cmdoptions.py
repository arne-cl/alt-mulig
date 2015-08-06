# usage
# py.test -q --cmdopt=type1
# py.test -q --cmdopt=type2
# py.test -q --cmdopt=foobar

def test_answer(cmdopt):
    if cmdopt == "type1":
        print ("first")
    elif cmdopt == "type2":
        print ("second")
    else:
	print(cmdopt)
    assert 0 # to see what was printed

