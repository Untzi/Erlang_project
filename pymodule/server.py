import socket
import sys
from PIL import Image, ImageOps, ImageDraw
import os
from clarifai.rest import ClarifaiApp
from clarifai.rest import Image as clImage
import thread
import hashlib

def listen_for_image(path,id,serv_ip,path_final):
    port = 6020
    s = socket.socket(socket.AF_INET,socket.SOCK_STREAM)
    host = socket.gethostname()
    s.bind((serv_ip.to_string(), port))            # change to ('', port) if outside of network
    s.listen(10)
    while 1:
#accept connections from outside
        (clientsocket, address) = s.accept()
        thread.start_new_thread(client_thread,(clientsocket,id,path,path_final))
        id+=1

def client_thread(soc,id,path,path_final):
    print path.to_string()
    fname = path.to_string()+str(id)+'.jpg'
    print 'going to save in' + fname
    with open(fname, 'wb') as f:
        while True:
            data = soc.recv(1024)
            if not data:
                break
            f.write(data)

    soc.close()
        #rate = getRate(fname)
    resize_crop_save(fname,path_final)


def resize_crop_save(img,path_final):
    hasher = hashlib.md5()
    with open(img, 'rb') as f:
        buf = f.read()
        hasher.update(buf)
    filehash = hasher.hexdigest()
    im = Image.open(img)
    im = im.resize((75, 75));
    save_path = path_final.to_string()+filehash[-50:]+'.png'
    print save_path
    im = im.save(save_path)
    #bigsize = (im.size[0] * 3, im.size[1] * 3)
    #mask = Image.new('L', bigsize, 0)
    #draw = ImageDraw.Draw(mask)
    #draw.ellipse((0, 0) + bigsize, fill=255)
    #mask = mask.resize(im.size, Image.ANTIALIAS)
    #im.putalpha(mask)
    #output = ImageOps.fit(im, mask.size, centering=(0.5, 0.5))
    #output.putalpha(mask)
    #output.save(path_final.to_string()+filehash[-50:]+'.png')
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
