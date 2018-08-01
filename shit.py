import os
import hashlib

if __name__=="__main__":
	files = os.listdir()
	ctr = 0
	for file_name in files:
		with open(file_name, 'rb') as f:
			buf = f.read()
			hasher.update(buf)
		filehash = hasher.hexdigest()
		os.rename(file_name, filehash[-32:]+'.png')