from http.client import HTTPSConnection
from base64 import b64encode
from json import loads
from json import dumps
import numpy as np
import random
import requests
from bs4 import BeautifulSoup
import json

class RestClientReviews:
    domain = "api.dataforseo.com"
    
    def __init__(self, username: str, password: str, lat, lon, keyword: str, depth: int=10):
        self.username = username
        self.password = password
        self.keyword = keyword
        self.depth = depth #number of reviews to0 return, multiples of 10
        self.lat = lat
        self.lon = lon

    def request(self, path, method, data=None):
        connection = HTTPSConnection(self.domain)
        try:
            base64_bytes = b64encode(
                ("%s:%s" % (self.username, self.password)).encode("ascii")
                ).decode("ascii")
            request_headers = {'Authorization' : 'Basic %s' %  base64_bytes, 'Content-Encoding' : 'gzip'}
            connection.request(method, path, headers=request_headers, body=data)
            response_request = connection.getresponse()
            return loads(response_request.read().decode())
        finally:
            connection.close()

    def get(self, path):
        return self.request(path, 'GET')

    def post(self, path, data):
        if isinstance(data, str):
            data_str = data
        else:
            data_str = dumps(data)
        return self.request(path, 'POST', data_str)
        
    def send_post(self):
        self.tag = "tiv-"+str(random.randint(1e7, 9.9e7))+"-"+str(random.randint(1e7, 9.9e7)) #generate random tag
        post_data = dict()
        loc_c = str(self.lat) + "," + str(self.lon) + ",200"
        post_data[len(post_data)] = dict(
            location_coordinate=loc_c,
            language_name="English",
            keyword=self.keyword,
            depth=self.depth,
            sort_by="newest",
            priority=2, #1 = normal priority, 2 = high priority
            tag=self.tag
        )
        response_post = self.post("/v3/reviews/google/task_post", post_data)
        # you can find the full list of the response codes here https://docs.dataforseo.com/v3/appendix/errors
        if response_post["status_code"] == 20000:
            return response_post
        else:
            print("error. Code: %d Message: %s" % (response["status_code"], response["status_message"]))
            
    def send_tasks(self, id_):
        response_tasks = self.get("/v3/reviews/google/task_get/" + id_)
        if response_tasks["status_code"] == 20000:
            filename = "temp/get/%s.json" % id_
            with open(filename, 'w') as outfile:
                json.dump(response_tasks, outfile)
            return(response_tasks)
        else:
            print("error. Code: %d Message: %s" % (response_tasks["status_code"], response_tasks["status_message"]))
            
class RestClientTrafficAnalytics:
    domain = "api.dataforseo.com"
    
    def __init__(self, username, password, target):
        self.username = username
        self.password = password
        self.target = target

    def request(self, path, method, data=None):
        connection = HTTPSConnection(self.domain)
        try:
            base64_bytes = b64encode(
                ("%s:%s" % (self.username, self.password)).encode("ascii")
                ).decode("ascii")
            request_headers = {'Authorization' : 'Basic %s' %  base64_bytes, 'Content-Encoding' : 'gzip'}
            connection.request(method, path, headers=request_headers, body=data)
            response_request = connection.getresponse()
            return loads(response_request.read().decode())
        finally:
            connection.close()

    def get(self, path):
        return self.request(path, 'GET')

    def post(self, path, data):
        if isinstance(data, str):
            data_str = data
        else:
            data_str = dumps(data)
        return self.request(path, 'POST', data_str)
        
    def send_post(self):
        self.tag = "tiv-"+str(random.randint(1e7, 9.9e7))+"-"+str(random.randint(1e7, 9.9e7)) #generate random tag
        post_data = dict()
        post_data[len(post_data)] = dict(
            target=self.target,
            priority=2, #1 = normal priority, 2 = high priority
            tag=self.tag
        )
        response_post = self.post("/v3/traffic_analytics/similarweb/task_post", post_data)
        # you can find the full list of the response codes here https://docs.dataforseo.com/v3/appendix/errors
        if response_post["status_code"] == 20000:
            return response_post
        else:
            print("error. Code: %d Message: %s" % (response["status_code"], response["status_message"]))
            
    def send_tasks(self, id_):
        response_tasks = self.get("/v3/traffic_analytics/similarweb/task_get/" + id_)
        if response_tasks["status_code"] == 20000:
            filename = "temp/get/%s.json" % id_
            with open(filename, 'w') as outfile:
                json.dump(response_tasks, outfile)
            return(response_tasks)
        else:
            print("error. Code: %d Message: %s" % (response_tasks["status_code"], response_tasks["status_message"]))
        
