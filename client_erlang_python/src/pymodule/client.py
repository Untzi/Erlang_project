
import socket 
import sys 

def clientSend(filePath,ip):
	filePath=filePath.to_string()
	ip=ip.to_string()
	s = socket.socket(socket.AF_INET,socket.SOCK_STREAM)            
	host = socket.gethostname()   
	port = 6000                   
	s.connect((ip, port))
	f = open(filePath,'rb')
	l = f.read(1024)
        while (l):
       		s.send(l)
	        l = f.read(1024)
    	f.close()
	s.close()
        print('Done sending')

if __name__ == '__main__':
	clientSend('//home//osboxes//Downloads//l-22703.jpg','osboxes')
