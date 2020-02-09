import re
import requests
import json
import time
from datetime import datetime
from fake_useragent import UserAgent
def saveUserData(steamID,userCount):
    savef = open("C:/Users/KTH/Desktop/read",'w')
    print(str(steamID))
    savef.write(str(steamID)+','+str(userCount)+'\n')
    savef.close()

def print_steam_crawl(steamID,ID_Range,fileNum):
    userCount = int(fileNum)
    for n in range(ID_Range):
        time.sleep(22)
        steamID = steamID+1
        string_ID = str(steamID)
        ua = UserAgent()
        headers = {'User-Agent':str(ua.random)}
        url = 'https://steamcommunity.com/profiles/'+string_ID+'/games/?tab=all&sort=playtime'
        try: #Private User Check
            html = requests.get(url,headers=headers).text
            if 'profile_fatalerror' in html:
                steamID = steamID-1
                saveUserData(steamID,userCount)
                now = datetime.now()
                writeLog = open("C:/Users/KTH/Desktop/log.txt",'a')
                writeLog.write('Error ID: '+str(steamID)+' Time: '+str(now)+'\n')
                writeLog.close()
                print("429 Error")
                time.sleep(1800)
                continue
            htmlSearch = re.search(r'var rgGames =(.+?);',html, re.S)
            gameList = htmlSearch.group(1)
            SteamGame = json.loads(gameList)
        except:
            saveUserData(steamID,userCount)
            continue
        if (str(SteamGame) != "[]" and 'hours_forever' in gameList):  #Private library Check
            userCount = userCount+1
            saveUserData(steamID,userCount)
            f = open('C:/Users/KTH/Desktop/'+str(userCount)+'.csv', 'w')
            f.write('GameName,Id:'+ string_ID+'\n')
            for course in SteamGame:
                try: #empty playtime Check
                    gameName = '{name}'.format(**course)
                    gameName = gameName.replace(',',' ') #if 'comma' inside GameName , replace 'space'
                    gameTime = '{hours_forever}'.format(**course)
                    gameTime = gameTime.replace(',','') #if 'comma' inside playtime, replace 'space'
                    f.write(gameName+','+gameTime+'\n')
                except:
                    continue
            f.close()
        else:
            saveUserData(steamID,userCount)
            continue

f = open("C:/Users/KTH/Desktop/read",'r')
data = f.read()
f.close()
saveData = data.split(',')
startID = int(saveData[0])
fileSave = int(saveData[1])
pages = 100000

now = datetime.now()
writeLog =  open("C:/Users/KTH/Desktop/log.txt",'a')
writeLog.write('Start ID: '+str(startID)+' Time: '+str(now)+'\n')
writeLog.close()

print_steam_crawl(startID,pages,fileSave)
#https://steamcommunity.com/profiles/76561197960265728/games/?tab=all&sort=playtime -> Test 429 Error
#76561197960265728 Steam First_User_number
