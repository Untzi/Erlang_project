import socket                   # Import socket module
import sys
from PIL import Image, ImageOps, ImageDraw
import os
from clarifai.rest import ClarifaiApp
from clarifai.rest import Image as clImage
import thread
import hashlib

def listen_for_image(path,id):
	port = 6000
	s = socket.socket(socket.AF_INET,socket.SOCK_STREAM)
	host = socket.gethostname()
	s.bind((host, port))            # change to ('', port) if outside of network
	s.listen(10)
	while 1:
    #accept connections from outside
		(clientsocket, address) = s.accept()
		thread.start_new_thread(client_thread,(clientsocket,id,path))
		id+=1

def client_thread(soc,id,path):
	fname = path+str(id)+'.jpg'
	with open(fname, 'wb') as f:
	    while True:
		data = soc.recv(1024)
		if not data:
		    break
		f.write(data)
	f.close()
	soc.close()
	rate = getRate(fname)
	resize_crop_save(fname)


def resize_crop_save(img):
	hasher = hashlib.md5()
	with open(img, 'rb') as f:
		buf = f.read()
		hasher.update(buf)
	filehash = hasher.hexdigest()
	im = Image.open(img)
	im = im.resize((50, 50));
	bigsize = (im.size[0] * 3, im.size[1] * 3)
	mask = Image.new('L', bigsize, 0)
	draw = ImageDraw.Draw(mask)
	draw.ellipse((0, 0) + bigsize, fill=255)
	mask = mask.resize(im.size, Image.ANTIALIAS)
	im.putalpha(mask)
	output = ImageOps.fit(im, mask.size, centering=(0.5, 0.5))
	output.putalpha(mask)
	output.save(filehash[-50:]+'.png')
    # output.save(os.path.splitext(img)[0]+'.png')
	os.remove(img)

def getRate(img):
	app = ClarifaiApp(api_key = 'b348f6da8d8744aea813cd459dfdf53b')
	model = app.models.get('e9576d86d2004ed1a38ba0cf39ecb4b1')
	image = clImage(file_obj = open(img,'rb'))
	pred = model.predict([image])
        sfw = pred['outputs'][0]['data']['concepts'][0]['value']
	nsfw = pred['outputs'][0]['data']['concepts'][1]['value']
	return {pred['outputs'][0]['data']['concepts'][0]['name']:pred['outputs'][0]['data']['concepts'][0]['value'],
	pred['outputs'][0]['data']['concepts'][1]['name']:pred['outputs'][0]['data']['concepts'][1]['value']
}


if __name__ == '__main__':
	if len(sys.argv)==1:
		listen_for_image('/home/osboxes/',1)
	else:
		accept_and_resize(sys.argv[1],sys.argv[2])

	# python server.py /home/osboxes/ 1
