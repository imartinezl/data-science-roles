#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Oct  7 10:32:54 2019

@author: imartinez
"""

import re
import json
import requests
from requests.exceptions import HTTPError
from bs4 import BeautifulSoup

def get_response_query(base_url, params={}):
    response = None
    try:
        rq_url = base_url
		if params:
		    rq_url = base_url + '?' + "&".join("=".join(_) for _ in params.items())
        response = requests.get(rq_url)
        # If the response was successful, no Exception will be raised
        response.raise_for_status()
    except HTTPError as http_err:
        print(f'HTTP error occurred: {http_err}')  # Python 3.6
    except Exception as err:
        print(f'Other error occurred: {err}')  # Python 3.6
    else:
        print('Success!')
    return response

def get_soup(base_url, params={}):
    response = get_response_query(base_url, params)
    soup = BeautifulSoup(response.text, 'html.parser')
    return soup

def grab_job_links(soup):
    """
    Grab all non-sponsored job posting links from a Indeed search result page using the given soup object
    
    Parameters:
        soup: the soup object corresponding to a search result page
                e.g. https://ca.indeed.com/jobs?q=data+scientist&l=Toronto&start=20
    
    Returns:
        urls: a python list of job posting urls
    
    """
    urls = []
    
    # Loop thru all the posting links
    for link in soup.find_all('div', {'class': 'title'}):
        # Since sponsored job postings are represented by "a target" instead of "a href", no need to worry here
        partial_url = link.a.get('href')
        # This is a partial url, we need to attach the prefix
        url = 'https://www.indeed.com' + partial_url
        # Make sure this is not a sponsored posting
        urls.append(url)
    
    return urls


def get_urls(base_url, params, num_pages):
    """
    Get all the job posting URLs resulted from a specific search.
    
    Parameters:
        query: job title to query
        num_pages: number of pages needed
        location: city to search in
    
    Returns:
        urls: a list of job posting URL's (when num_pages valid)
        max_pages: maximum number of pages allowed ((when num_pages invalid))
    """
    # We always need the first page
    soup = get_soup(base_url, params)
    urls = grab_job_links(soup)
    
    # Get the total number of postings found 
    posts_count_string = soup.find(name='div', attrs={'id':"searchCountPages"}).get_text()
    posts_count_string = posts_count_string[posts_count_string.find('of')+2:posts_count_string.find('jobs')].strip()
    posts_count_string = re.sub(r',', '', posts_count_string)
    #print('posts_count_string: {}'.format(posts_count_string))
    #print('type is: {}'.format(type(posts_count_string)))
    try:
        posts_count = int(posts_count_string)
    except ValueError: # deal with special case when parsed string is "360 jobs"
        posts_count = int(re.search('\d+', posts_count_string).group(0))
        #print('posts_count: {}'.format(posts_count))
        #print('\ntype: {}'.format(type(posts_count)))
    finally:
        #posts_count = 330 # setting to 330 when unable to get the total
        pass
    
    # Limit nunmber of pages to get
    max_pages = round(posts_count / 10) - 3
    if num_pages > max_pages:
        print('returning max_pages!!')
        return max_pages
    
        # Additional work is needed when more than 1 page is requested
    if num_pages >= 2:
        # Start loop from page 2 since page 1 has been dealt with above
        for i in range(2, num_pages+1):
            num = (i-1) * 10
            p = params
            p['start'] = str(num)
            try:
                soup = get_soup(base_url, p)
                # We always combine the results back to the list
                urls += grab_job_links(soup)
            except:
                continue
    # Check to ensure the number of urls gotten is correct
    #assert len(urls) == num_pages * 10, "There are missing job links, check code!"
    return urls



def get_posting(url):
    """
    Get the text portion including both title and job description of the job posting from a given url
    
    Parameters:
        url: The job posting link
        
    Returns:
        title: the job title (if "data scientist" is in the title)
        posting: the job posting content    
    """
    # Get the url content as BS object
    soup = get_soup(url)
    
    # The job title is held in the h3 tag
    title = soup.find(name='h3').getText().lower()
    posting = soup.find(name='div', attrs={'class': "jobsearch-JobComponent"}).get_text()
    return title, posting.lower()
        
    #if 'data scientist' in title:  # We'll proceed to grab the job posting text if the title is correct
        # All the text info is contained in the div element with the below class, extract the text.
        #posting = soup.find(name='div', attrs={'class': "jobsearch-JobComponent"}).get_text()
        #return title, posting.lower()
    #else:
        #return False
    
        # Get rid of numbers and symbols other than given
        #text = re.sub("[^a-zA-Z'+#&]", " ", text)
        # Convert to lower case and split to list and then set
        #text = text.lower().strip()
    
        #return text



def get_data(base_url, params, num_pages=1):
    """
    Get all the job posting data and save in a json file using below structure:
    
    {<count>: {'title': ..., 'posting':..., 'url':...}...}
    
    The json file name has this format: ""<query>.json"
    
    Parameters:
        query: Indeed query keyword such as 'Data Scientist'
        num_pages: Number of search results needed
        location: location to search for
    
    Returns:
        postings_dict: Python dict including all posting data
    
    """
    postings_dict = {}
    urls = get_urls(base_url, params, num_pages)
    
    #  Continue only if the requested number of pages is valid (when invalid, a number is returned instead of list)
    if isinstance(urls, list):
        num_urls = len(urls)
        for i, url in enumerate(urls):
            try:
                title, posting = get_posting(url)
                postings_dict[i] = {}
                postings_dict[i]['title'], postings_dict[i]['posting'], postings_dict[i]['url'] = \
                title, posting, url
            except: 
                continue
            
            percent = (i+1) / num_urls
            # Print the progress the "end" arg keeps the message in the same line 
            print("Progress: {:2.0f}%".format(100*percent), end='\r')
        # Save the dict as json file
        file_name = query.replace('+', '_') + '.json'
        with open(file_name, 'w') as f:
            json.dump(postings_dict, f)
        
        print('All {} postings have been scraped and saved!'.format(num_urls))    
        #return postings_dict
    else:
        print("Due to similar results, maximum number of pages is only {}. Please try again!".format(urls))



# If script is run directly, we'll take input from the user
if __name__ == "__main__":
    # queries = ["data scientist", "machine learning engineer", "data engineer"]
    
    # while True: 
    #     query = input("Please enter the title to scrape data for: \n").lower()
    #     if query in queries:
    #         break
    #     else:
    #         print("Invalid title! Please try again.")

    # while True:
    #     num_pages = input("Please enter the number of pages needed (integer only): \n")
    #     try:
    #         num_pages = int(num_pages)
    #         break
    #     except:
    #         print("Invalid number of pages! Please try again.")

query = 'data scientist'
query = '+'.join(query.lower().split())
base_url = 'https://www.indeed.com/'
# params={'q': 'data-scientist', 'l': 'Toronto'}
params={'q': query}
num_pages = 1

get_data(base_url, params, num_pages)

 