import os
import hashlib
from PIL import Image, ImageOps, ImageDraw
def resize_crop_save(img):
	hasher = hashlib.md5()
	with open(img, 'rb') as f:
		buf = f.read()
		hasher.update(buf)
	filehash = hasher.hexdigest()
	im = Image.open(img)
	im = im.resize((50, 50));
	im.save(filehash[-50:]+'.png')
    # output.save(os.path.splitext(img)[0]+'.png')
	os.remove(img)

if __name__=="__main__":
	files = os.listdir('C:\\Users\\Administrator\\Downloads\\for_ybs')
	ctr = 0
	for file_name in files:
		resize_crop_save(file_name)
		