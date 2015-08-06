# Pass different values to a test function, depending on command line options
# ---------------------------------------------------------------------------

# To use test with cli options, we need to add a command line option and
# provide the cmdopt through a fixture function:

# cf. https://pytest.org/latest/example/simple.html
import pytest

def pytest_addoption(parser):
    parser.addoption("--cmdopt", action="store", default="type1",
        help="my option: type1 or type2")

@pytest.fixture
def cmdopt(request):
    return request.config.getoption("--cmdopt")


# Dynamically adding command line options
# ---------------------------------------

# Through addopts you can statically add command line options for your project.
# You can also dynamically modify the command line arguments before they get processed:

import sys
def pytest_cmdline_preparse(args):
    if 'xdist' in sys.modules: # pytest-xdist plugin
        import multiprocessing
        num = max(multiprocessing.cpu_count() / 2, 1)
        args[:] = ["-n", str(num)] + args

# If you have the xdist plugin installed you will now always perform test runs using a
# number of subprocesses close to your CPU.



