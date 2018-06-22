#!/usr/bin/python3

from tmuxp  import cli
from string import Template

import crayons
import socket
import sys
import requests

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

def register_keygen(cfg):
    url = http_template.format(servername=cfg['servername'], port = cfg['keygenport'], endpoint='register.html')
    r = requests.post(url)
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


start_servers(muxconfig)
print("Started server")
check_server(muxconfig['bbport'], 'bulletin board')
reset_bulletin_board(muxconfig)
check_server(muxconfig['keygenport'], 'keygen server')
register_keygen(muxconfig)
check_server(muxconfig['dbport'], 'database server')
check_server(muxconfig['controllerport'], 'controller server')

