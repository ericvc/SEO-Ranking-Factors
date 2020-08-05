import requests
from bs4 import BeautifulSoup
import re
import numpy as np
import time as time
import pandas as pd

class coord_scraper:

    def __init__(self, city, state, sleep=1, platform="desktop"):
        self.city = city
        self.state = state
        self.query = city.replace(" ", "_") + ",_" + state.replace(" ","_")
        self.url = "https://www.wikipedia.org/wiki/%s" % self.query
        self.platform = platform
        self.coords = None
        self.sleep = sleep

    def get_coords(self):

        USER_AGENT_HEADERS = {"user-agent":"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:65.0) Gecko/20100101 Firefox/65.0"}

        #GET HTML response
        response = requests.get(self.url, headers=USER_AGENT_HEADERS)

        # if response status code is 200
        if response:
          try:
              #Parse HTML response
              soup = BeautifulSoup(response.text, 'html.parser')
              #Locate and extract longitudinal coordinates
              latlon = soup.find("span", class_="geo-dec").text
              #Convert coordinates digits to floats and store in a numpy array, using list comprehension
              latlon_dd = np.array([float(i) for i in re.findall("\\d{1,}[.]\\d{1,}", latlon)])
              if re.findall("E|W", latlon)[0] == "W":
                  latlon_dd[1] = -1 * latlon_dd[1] #negative sign if in the Western hemisphere
              if re.findall("S|N", latlon)[0] == "S":
                  latlon_dd[0] = -1 * latlon_dd[0] #negative sign if in the Southern hemisphere
              self.lat = latlon_dd[0]
              self.lon = latlon_dd[1]
              self.coords = {"latitude":self.lat, "longitude":self.lon}
          except AttributeError:
              print("WARNING: Webpage did not contain coordinate locations (e.g., search yielded a disambiguation page.")
              self.lat = np.nan
              self.lon = np.nan
              self.coords = {"longitude":np.nan, "latitude":np.nan}
        else:
            print("WARNING: Webpage response status code did not return 200 (STATUS: %d)" % response.status_code)
            self.lat = np.nan
            self.lon = np.nan
            self.coords = {"longitude":np.nan, "latitude":np.nan}
