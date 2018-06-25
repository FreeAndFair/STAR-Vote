#!/usr/bin/python3

from tmuxp  import cli
from string import Template

import crayons
import socket
import sys
import requests
import os
import textwrap

startPort = 8000
muxconfig = { 'parentdir'      : '~'
            , 'dir'            : 'starvote'
            , 'bbport'         : startPort
            , 'keygenport'     : startPort + 1
            , 'dbport'         : startPort + 2
            , 'controllerport' : startPort + 3
            , 'servername'     : 'localhost'
            }



def start_servers(config):

    with open('serversession.yaml', 'r') as filein:
        template = Template (filein.read())

    subst = template.substitute(muxconfig)

    with open('serversession_filled.yaml', 'w',) as fileout:
        print(subst, file=fileout)

    cli.load_workspace('serversession_filled.yaml', detached=True)

def check_server(port, nickname, servername='localhost'):
    sock = socket.socket()
    result = sock.connect_ex((servername, port))
    print_item('Testing connection to {}'.format(nickname), code_to_string(result))
    return result

def code_to_string(code):
    if code == 0:
        return crayons.green('Connected', bold = True)
    else:
        return crayons.red('Failed', bold = True) + crayons.red(' with code: ' + str(code))

def http_resposne_to_string(resp):
    if resp.ok:
        return crayons.green(resp.reason, bold = True)
    else:
        return crayons.red('Error ' + resp.reason, bold = True)

def print_item(item, response):
    print('{it:<{padding}} {response}'.format(it=item, padding=60, response=response))

http_template = 'http://{servername}:{port!s}/{endpoint}'

def initialize_keygen(cfg, trustees, threshold):
    url = http_template.format(servername=cfg['servername'], port = cfg['keygenport'], endpoint='initialize.html')
    values = {'trustee_count':trustees, 'threshold':threshold}    
    r=requests.post(url, data=values)
    print_item('Initialzing key', http_resposne_to_string(r))
    filename = '{pd}/{filename}/key.html'.format(pd=cfg['parentdir'], filename=cfg['dir'])
    with open(os.path.expanduser(filename),'w+') as fileout:
        print(r.text, file=fileout)
    print_item('Writing key to {fn}'.format(fn=filename), crayons.green('OK', bold=True))

def register_keygen(cfg):
    url = http_template.format(servername=cfg['servername'], port = cfg['keygenport'], endpoint='register.html')
    values = {'action':'register and refetch'} #we want to refetch, since we've reset the bb since startup
    r = requests.post(url, data = values)
    print_item ('Registering with keygen server', http_resposne_to_string(r))

def get_reset_page(cfg):
    url = http_template.format(servername=cfg['servername'], port = cfg['bbport'], endpoint='reset')
    r = requests.get(url, headers={'referer': 'http://localhost:8000/'})
    print_item('Getting reset page', http_resposne_to_string(r))

def post_reset_bulletin_board(cfg):
    url = http_template.format(servername=cfg['servername'], port = cfg['bbport'], endpoint='reset')
    r = requests.post(url)
    print_item('Resetting bulletin board', http_resposne_to_string(r))

def reset_bulletin_board(cfg):
    get_reset_page(cfg)
    post_reset_bulletin_board(cfg)

def initialize_voters(cfg, csv):
    url = http_template.format(servername=cfg['servername'], port = cfg['dbport'], endpoint='initialize')
    r = requests.post(url,data={'csv':csv})
    print_item('Initializing voter database', http_resposne_to_string(r))

voters = textwrap.dedent('\
            1,John Doe,Nowhereland,1,oregon-201\n\
            2,Jane Doe,Stix,2,oregon-2014\n\
            3,Jimmy Bat,Stix,3,oregon-2014')

start_servers(muxconfig)
print("Started server")
check_server(muxconfig['bbport'], 'bulletin board')
reset_bulletin_board(muxconfig)
check_server(muxconfig['keygenport'], 'keygen server')
register_keygen(muxconfig)
initialize_keygen(muxconfig,1,1)
check_server(muxconfig['dbport'], 'database server')
initialize_voters(muxconfig,voters)
check_server(muxconfig['controllerport'], 'controller server')

