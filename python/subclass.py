
class NameRecord(object):
	def __init__(self, name):
		self.name = name

	@classmethod
	def from_string(cls, record_string):
		fields = record_string.split('\t')
		return cls(name=fields[0])

	def __str__(self):
		return '\n'.join('{}: {}'.format(k,v)
                                 for k,v in self.__dict__.items())

class FullNameRecord(NameRecord):
	def __init__(self, name, first_name):
		super(FullNameRecord, self).__init__(name)
		self.first_name = first_name

	@classmethod
	def from_string(cls, record_string):
		fields = record_string.split('\t')
		return cls(name=fields[0], first_name=fields[1])


if __name__ == '__main__':
	record = 'Doe\tJohn\tNYC'
	print NameRecord('Doe')
	print NameRecord.from_string(record)
	print FullNameRecord('Doe', 'John')
	print FullNameRecord.from_string(record)
