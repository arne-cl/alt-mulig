import unittest
from unittest.mock import patch

class Student(object):
    def __init__(self, name):
        self.name = name

    def get_name(self):
        return self.name


class TestStudent(unittest.TestCase):
    def test_get_name(self):
        horst = Student('horst')
        self.assertEqual('horst', horst.get_name())
    
#    @patch.object(Student, 'get_name', return_value='bibi')
    @patch.object(Student)
    def test_get_name_patched(self, patched_student):
        import pudb; pudb.set_trace()
        horst = patched_student('horst')
        self.assertEqual('bibi', horst.get_name())


#    @patch.object(requests, 'get', side_effect=requests.exceptions.Timeout)
#    def test_get_holidays_timeout(self, mock_requests):
#            with self.assertRaises(requests.exceptions.Timeout):
#                get_holidays()

if __name__ == '__main__':
    unittest.main()

