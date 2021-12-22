from datetime_example import BaseSoilMeasurement, get_report_month_key
# ~ import mock
from unittest import mock
import unittest

from datetime import datetime


@mock.patch('datetime_example.datetime.datetime')
def test_dt(mock_dt):
    mock_dt.utcnow = mock.Mock(return_value=datetime(1901, 12, 21))
    r = get_report_month_key()
    assert '190112' == r


@mock.patch('datetime_example.datetime.datetime')
def test_get_unique_id(mock_dt):    
    mock_dt.utcnow = mock.Mock(return_value=datetime(2020, 1, 1))
    device_id = 0
    
    bsm = BaseSoilMeasurement(device_id=device_id)
    unique_id_dict = bsm.get_unique_id()
    assert unique_id_dict['device_id'] == device_id
    assert unique_id_dict['timestamp'] == '2020-01-01T00:00:00+00:00'
