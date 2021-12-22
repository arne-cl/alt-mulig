# ~ from datetime import datetime
import datetime

def get_report_month_key():
    month_for_report = datetime.datetime.utcnow()
    return month_for_report.strftime("%Y%m") 


class BaseSoilMeasurement(object):
    def __init__(self, device_id: int = -1):
        self.device_id = device_id
        self.timestamp = datetime.datetime.utcnow().replace(
            tzinfo=datetime.timezone.utc).replace(microsecond=0).isoformat()

    def get_unique_id(self):
        return {
            'device_id': self.device_id,
            'timestamp': self.timestamp
        }


