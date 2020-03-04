from mock import patch


class Person(object):
    def __init__(self, name):
        self.name = name

    def get_name(self):
        return self.name


def test_get_name_unpatched():
    tmp = Person('Horst')
    assert tmp.get_name() == 'Horst'


@patch.object(Person, 'get_name')
def test_get_name(get_name_mock):
    get_name_mock.return_value = 'Bibi'
    tmp = Person('Horst')
    assert tmp.get_name() == 'Bibi'


def test_get_name_with_context():
    expected_name = 'Horst'
    
    tmp = Person(expected_name)
    assert tmp.get_name() == expected_name
    
    with patch.object(Person, 'get_name') as get_name_mock:
        get_name_mock.return_value = 'Bibi'
        tmp = Person(expected_name)
        assert tmp.get_name() == 'Bibi'

    tmp = Person(expected_name)
    assert tmp.get_name() == expected_name


def test_get_name_with_context_and_return_value():
    expected_name = 'Horst'
    tmp = Person(expected_name)
    assert tmp.get_name() == expected_name
    
    with patch.object(Person, 'get_name', return_value = 'Bibi') as get_name_mock:
        tmp = Person(expected_name)
        assert tmp.get_name() == 'Bibi'

    tmp = Person(expected_name)
    assert tmp.get_name() == expected_name
