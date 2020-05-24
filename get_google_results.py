#Code adapted from: https://hackernoon.com/how-to-scrape-google-with-python-bo7d2tal
import requests
from bs4 import BeautifulSoup
import pandas as pd
from sys import argv
#import datetime

class google_results:
    def __init__(self, query, num_results=20, platform="desktop"):
        self.query = query.replace(" ", "+")
        self.num_results = num_results
        self.url = f"https://www.google.com/search?q={self.query}&num={self.num_results}"
        self.platform = platform
        self.results = []

    def run(self):

        ## define device to simluate (default = desktop)
        # desktop user-agent
        USER_AGENT = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:65.0) Gecko/20100101 Firefox/65.0"
        # mobile user-agent
        MOBILE_USER_AGENT = "Mozilla/5.0 (Linux; Android 7.0; SM-G930V Build/NRD90M) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/59.0.3071.125 Mobile Safari/537.36"
        if self.platform == "desktop":
            headers = {"user-agent": USER_AGENT}
        elif self.platform == "mobile":
            headers = {"user-agent": MOBILE_USER_AGENT}

        #Previously, ran into trouble getting query without headers
        #This bit of code proved crucial. See website at top for more details.
        response = requests.get(self.url, headers=headers)

        if response.status_code == 200:
            soup = BeautifulSoup(response.text, 'html.parser')
            for g in soup.find_all('div', class_='r'):
                anchors = g.find_all('a')
                if anchors:
                    link = anchors[0]['href']
                    title = g.find('h3').text
                    item = {
                        "title": title,
                        "link": link
                    }
                    self.results.append(item)
            self.results = pd.DataFrame(self.results) #format as dataframe for export
            # The code below adds functionality for exporting the results to a .csv file
            # ts = datetime.datetime.utcnow()
            # self.results.to_csv("cache/"+self.query+"_"+ts.strftime("%Y-%b-%d")+"_.csv")
        else:
            print(f"ERROR: Webpage response status code did not return 200 ({response.status_code})")
