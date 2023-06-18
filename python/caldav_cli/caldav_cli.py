import argparse
import caldav
import getpass
from icalendar import Calendar
from datetime import datetime


def main(url, username, password=None):
    # ask for password if it wasn't passed as a parameter
    if password is None:
        password = getpass.getpass()

    # Connect to the CalDAV server and fetch the calendars
    client = caldav.DAVClient(url=url, username=username, password=password)
    principal = client.principal()
    calendars = principal.calendars()

    print(f"{len(calendars)} calendar(s) found.\n")

    # If there are any calendars
    for i, calendar in enumerate(calendars):
        events = calendar.events()
        print(f"\nCalendar {i} has {len(events)} events.\n")
        for event in events:
            ical = Calendar.from_ical(event.data)
            # Iterate over the components
            for component in ical.walk():
                if component.name == "VEVENT":
                    # If it's an event, print its start date
                    dt_start = component.get('dtstart').dt
                    # dtstart could be either date or datetime. Convert to date if necessary.
                    if isinstance(dt_start, datetime):
                        dt_start = dt_start.date()

                    # Get the event title
                    event_title = component.get('summary')

                    # Print the date and title of the event
                    print(f"Date: {dt_start}, Event: {event_title}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Extract dates from a CalDav calendar.')
    parser.add_argument('-u', '--url', required=True, help='CalDav server URL')
    parser.add_argument('-n', '--username', required=True, help='Username for the CalDav server')
    parser.add_argument('-p', '--password', help='Password for the CalDav server')
    
    args = parser.parse_args()
    
    main(args.url, args.username, args.password)

