
from __future__ import print_function # py 2.7 compat.
import base64

from IPython.html import widgets # Widget definitions.
from IPython.utils.traitlets import Unicode # Traitlet needed to add synced attributes to the widget.


class FileWidget(widgets.DOMWidget):
    """
    This is a custom widget that allows the user to upload file data to the
    notebook server. The file data is sent via a statefull `value` attribute
    of the widget. The widget has an upload failed event that fires in the
    front-end and is echoed to the back-end using a custom msg.
    """
    _view_name = Unicode('FilePickerView', sync=True)
    value = Unicode(sync=True)
    filename = Unicode(sync=True)

    def __init__(self, **kwargs):
        """Constructor"""
        widgets.DOMWidget.__init__(self, **kwargs) # Call the base.

        # Allow the user to register error callbacks with the following signatures:
        #    callback()
        #    callback(sender)
        self.errors = widgets.CallbackDispatcher(accepted_nargs=[0, 1])

        # Listen for custom msgs
        self.on_msg(self._handle_custom_msg)

    def _handle_custom_msg(self, content):
        """Handle a msg from the front-end.

        Parameters
        ----------
        content: dict
            Content of the msg."""
        if 'event' in content and content['event'] == 'error':
            self.errors()
            self.errors(self)


if __name__ == '__main__':

    # The following shows how the file widget can be used.

    file_widget = FileWidget()

    # Register an event to echo the filename when it has been changed.
    def file_loading():
        print("Loading %s" % file_widget.filename)
    file_widget.on_trait_change(file_loading, 'filename')

    # Register an event to echo the filename and contents when a file
    # has been uploaded.
    def file_loaded():
        print("Loaded, file contents: %s" % file_widget.value)
    file_widget.on_trait_change(file_loaded, 'value')

    # Register an event to print an error message when a file could not
    # be opened.  Since the error messages are not handled through
    # traitlets but instead handled through custom msgs, the registration
    # of the handler is different than the two examples above.  Instead
    # the API provided by the CallbackDispatcher must be used.
    def file_failed():
        print("Could not load file contents of %s" % file_widget.filename)
    file_widget.errors.register_callback(file_failed)

    file_widget

