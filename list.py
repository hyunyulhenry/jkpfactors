from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from bs4 import BeautifulSoup
import pandas as pd
import time

driver = webdriver.Chrome()
url = 'https://jkpfactors.com/'
driver.get(url)

time.sleep(3)

# Region/Country
driver.find_element(By.XPATH, '//*[@id="__next"]/div/div[1]/main/div[1]/div/div[1]/div/div/div').click()
data_region = driver.find_elements(By.CLASS_NAME, 'MuiMenuItem-root.MuiMenuItem-gutters.MuiButtonBase-root.css-mli2p5')
time.sleep(1)

list_region = []
for item in data_region:
    list_region.append(item.text)

data_region_name = driver.find_elements(By.CLASS_NAME, 'MuiMenuItem-root.MuiMenuItem-gutters.MuiButtonBase-root.css-mli2p5')    
list_region_name = []
for item in data_region_name:
    list_region_name.append(item.get_attribute('data-value'))
    
df_region = pd.DataFrame({
    'region': list_region,
    'query' : list_region_name})    

time.sleep(2)
driver.find_element(By.CSS_SELECTOR, '.MuiList-root.MuiList-padding.MuiMenu-list.css-r8u8y9').send_keys(Keys.ESCAPE)

# Theme/Factor
driver.find_element(By.XPATH, '//*[@id="__next"]/div/div[1]/main/div[1]/div/div[2]/div/div/div').click()
data_factor = driver.find_elements(By.CLASS_NAME, 'MuiMenuItem-root.MuiMenuItem-gutters.MuiButtonBase-root.css-mli2p5')
time.sleep(1)

list_factor = []
for item in data_factor:
    list_factor.append(item.text)
    
data_factor_name = driver.find_elements(By.CLASS_NAME, 'MuiMenuItem-root.MuiMenuItem-gutters.MuiButtonBase-root.css-mli2p5')    

list_factor_name = []
for item in data_factor_name:
    list_factor_name.append(item.get_attribute('data-value'))
    
df_factor = pd.DataFrame({
    'factor': list_factor,
    'query' : list_factor_name})    

time.sleep(2)
driver.find_element(By.CSS_SELECTOR, '.MuiList-root.MuiList-padding.MuiMenu-list.css-r8u8y9').send_keys(Keys.ESCAPE)

# Frequency
driver.find_element(By.XPATH, '//*[@id="__next"]/div/div[1]/main/div[1]/div/div[3]/div/div/div').click()
data_freq = driver.find_elements(By.CLASS_NAME, 'MuiMenuItem-root.MuiMenuItem-gutters.MuiButtonBase-root.css-mli2p5')
time.sleep(1)

list_freq = []
for item in data_freq:
    list_freq.append(item.text)
    
data_freq_name = driver.find_elements(By.CLASS_NAME, 'MuiMenuItem-root.MuiMenuItem-gutters.MuiButtonBase-root.css-mli2p5')    

list_freq_name = []
for item in data_freq_name:
    list_freq_name.append(item.get_attribute('data-value'))
    
df_freq = pd.DataFrame({
    'frequency': list_freq,
    'query' : list_freq_name})    

time.sleep(2)
driver.find_element(By.CSS_SELECTOR, '.MuiList-root.MuiList-padding.MuiMenu-list.css-r8u8y9').send_keys(Keys.ESCAPE)

# Weighting
driver.find_element(By.XPATH, '//*[@id="__next"]/div/div[1]/main/div[1]/div/div[4]/div/div/div').click()
data_wt = driver.find_elements(By.CLASS_NAME, 'MuiMenuItem-root.MuiMenuItem-gutters.MuiButtonBase-root.css-mli2p5')
time.sleep(1)

list_wt = []
for item in data_wt:
    list_wt.append(item.text)
    
data_wt_name = driver.find_elements(By.CLASS_NAME, 'MuiMenuItem-root.MuiMenuItem-gutters.MuiButtonBase-root.css-mli2p5')    

list_wt_name = []
for item in data_wt_name:
    list_wt_name.append(item.get_attribute('data-value'))
    
df_wt = pd.DataFrame({
    'weight': list_wt,
    'query' : list_wt_name})    

time.sleep(2)
driver.find_element(By.CSS_SELECTOR, '.MuiList-root.MuiList-padding.MuiMenu-list.css-r8u8y9').send_keys(Keys.ESCAPE)

#--------------#

with pd.ExcelWriter('list.xlsx') as writer:
    df_region.to_excel(writer, sheet_name='region', index = False)
    df_factor.to_excel(writer, sheet_name='factor', index = False)
    df_freq.to_excel(writer, sheet_name='frequency', index = False)
    df_wt.to_excel(writer, sheet_name='weight', index = False)

