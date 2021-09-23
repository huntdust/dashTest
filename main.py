
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from datetime import date


path1 = r'\\pilly\Advanced_Signal_Delivery\Personal_Folders\Dustin\tempData_ambient.txt'
path2 = r'\\pilly\Advanced_Signal_Delivery\Personal_Folders\Dustin\tempData_reliability.txt'
path3 = r'\\pilly\Advanced_Signal_Delivery\Personal_Folders\Dustin\tempData_gryffindor.txt'
path4 = r'\\pilly\Advanced_Signal_Delivery\Personal_Folders\Dustin\tempData_HPambient.txt'
path5 = r'\\pilly\Advanced_Signal_Delivery\Personal_Folders\Dustin\tempData_Leak1.txt'
path6 = r'\\pilly\Advanced_Signal_Delivery\Personal_Folders\Dustin\tempData_Leak2.txt'
thermPath = 'C:/Users/huntdust/Desktop/Temperature Data/Temp/ThermoData.txt'
#path1 = 'C:/Users/huntdust/Desktop/Temperature Data/Temp/tempData_ambient.txt'
#path2 = 'C:/Users/huntdust/Desktop/Temperature Data/Temp/tempData_reliability.txt'
#path3 = 'C:/Users/huntdust/Desktop/Temperature Data/Temp/tempData_gryffindor.txt'

statPath = r'C:\Users\huntdust\Desktop\variationData.txt'
stat2Path = r'\\pilly\Advanced_Signal_Delivery\Personal_Folders\Dustin\variationData_2.txt'
stat3Path = r'\\pilly\Advanced_Signal_Delivery\Personal_Folders\Dustin\variationData_calibration_2.txt'

def plot_temps():
    ambient = pd.read_csv(path1, sep='\t')
    reliability = pd.read_csv(path2, sep='\t')
    gryffindor = pd.read_csv(path3, sep = '\t')
    HP = pd.read_csv(path4, sep = '\t')
    HP2 = pd.read_csv(path5, sep ='\t')
    HP3 = pd.read_csv(path6, sep ='\t')

    ambientTemp = ambient.iloc[:,[1]]
    ambientHum = ambient.iloc[:,[2]]
    reliabilityTemp = reliability.iloc[:,[1]]
    reliabilityHum = reliability.iloc[:,[2]]
    gryffindorTemp = gryffindor.iloc[:,[1]]
    gryffindorHum = gryffindor.iloc[:,[2]]
    HPTemp,HPTemp2,HPTemp3 = HP.iloc[:,[1]], HP2.iloc[:,[1]], HP3.iloc[:,[1]]
    HPHum, HPHum2, HPHum3 = HP.iloc[:,[2]], HP2.iloc[:,[2]], HP3.iloc[:,[2]]

    time = reliability.iloc[:,[0]]
    datapoints = len(time)

    ##################################################### Arrange HP data with padded zeros for appropriate visualization

    zeros = datapoints - len(HPTemp)
    ZeroCol = [0] * zeros

    HPTemp, HPTemp2, HPTemp3 = HPTemp.to_numpy(), HPTemp2.to_numpy(), HPTemp3.to_numpy()
    HPTemp, HPTemp2, HPTemp3 = HPTemp[:,0], HPTemp2[:,0], HPTemp3[:,0]
    HPTemp, HPTemp2, HPTemp3 = np.concatenate((ZeroCol,HPTemp)), np.concatenate((ZeroCol,HPTemp2)), np.concatenate((ZeroCol,HPTemp3))       #temps + zeros
    HPTemp, HPTemp2, HPTemp3 = pd.DataFrame(HPTemp), pd.DataFrame(HPTemp2), pd.DataFrame(HPTemp3)

    HPHum,  HPHum2,  HPHum3 =  HPHum.to_numpy(), HPHum2.to_numpy(), HPHum3.to_numpy()
    HPHum,  HPHum2,  HPHum3 = HPHum[:,0], HPHum2[:,0], HPHum3[:,0]
    HPHum,  HPHum2,  HPHum3 = np.concatenate((ZeroCol,HPHum)), np.concatenate((ZeroCol,HPHum2)), np.concatenate((ZeroCol,HPHum3))       #temps + zeros
    HPHum,  HPHum2,  HPHum3 = pd.DataFrame(HPHum), pd.DataFrame(HPHum2), pd.DataFrame(HPHum3)

    ##################################################### make datelist
    date1 = '07/07/2021'
    finaldate = date.today().strftime("%m/%d")

    datelist = [datetime.strftime('%m/%d') for datetime in pd.date_range(date1, periods=int(np.ceil(datapoints/288))).tolist()]

    ######################################################

    temps = pd.concat([time, ambientTemp, reliabilityTemp, gryffindorTemp, HPTemp, HPTemp2, HPTemp3], axis=1) #, HPTemp, HPTemp2, HPTemp3
    hums  = pd.concat([time, ambientHum, reliabilityHum, gryffindorHum, HPHum,  HPHum2,  HPHum3], axis = 1)


    temps.columns = ['time','Ambient', 'Reliability', 'Gryffindor', 'HPAmbient', 'HPLeak O', 'HPLeak I'] #, 'HP ambient', 'HP Leakage outside', 'HP Leakage inside'
    hums.columns = ['time','Ambient', 'Reliability', 'Gryffindor', 'HPAmbient', 'HPLeak O', 'HPLeak I']    #Set column labelling
    ax = temps.plot(x='time',rot=45,ylim=(20,35)).set_title('Signal Delivery Lab Temperature (C)')
    locs, labels = plt.xticks()

    #calculate and arrange xticks so that interval is one day
    days = np.floor(datapoints/288)
    intervals = list(range(0,datapoints,288))
    timeList = [None]
    for i in range(int(np.ceil(datapoints/288))):                 #number of days
        timeList.append('2:35 PM ' + datelist[i])
    timeList.remove(None)
    #
    plt.xticks(intervals,timeList)
    plt.tight_layout()
    plt.savefig('tempPlot.png')
    

    ax2 = hums.plot(x='time',rot=45,ylim=(20,70)).set_title('Signal Delivery Lab Humidity (%RH)')
    plt.xticks(intervals, timeList)
    plt.tight_layout()
    plt.show()
    plt.savefig('humPlot.png')

    plt.clf()
    return plt

def thermData():
    therm = pd.read_csv(thermPath, sep ='\t')

    thermTemp = therm.iloc[:,[1]]
    time = therm.iloc[:,[0]]

    data = pd.concat([time, thermTemp],axis=1)
    data.columns = ['time', 'thermTemp']

    plot = data.plot(x='time')
    plt.show()


def temp_stats():
    tempStats = pd.read_csv(stat3Path)
    temps = tempStats.iloc[:,[2,4,6,8]]
    hums = tempStats.iloc[:,[3,5,7,9]]
    tempDiffs = tempStats.iloc[:,[10,11,12]]
    humDiffs = (tempStats.iloc[:,[13,14,15]])

    avgTempVariation = np.average(abs(tempDiffs))
    avgHumVariation = np.average(humDiffs)

    maxTempVariation = np.max(tempDiffs)
    maxHumVariation = np.max(humDiffs)

    minTempVariation = np.min(tempDiffs)
    minHumVariation = np.min(humDiffs)

    tempstd = np.std(tempDiffs)
    humstd = np.std(humDiffs)

    Tempavg1 = np.average(tempDiffs.iloc[:,[0]])
    Tempavg2 = np.average(tempDiffs.iloc[:,[1]])
    Tempavg3 = np.average(tempDiffs.iloc[:,[2]])
    avgs = [Tempavg1, Tempavg2, Tempavg3]
    print(avgs, maxTempVariation,minTempVariation,tempstd, '\n\n')
    #print(avgTempVariation, 'C')




    Humavg1 = np.average(humDiffs.iloc[:,[0]])
    Humavg2 = np.average(humDiffs.iloc[:,[1]])
    Humavg3 = np.average(humDiffs.iloc[:, [2]])
    humavgs = [Humavg1, Humavg2, Humavg3]
    print(humavgs,maxHumVariation,minHumVariation,humstd)


    temps.plot().set_title('Temps')
    hums.plot().set_title('Hums')
    tempDiffs.plot().set_title('Temperature Deviation Between Sensors (C)')
    humDiffs.plot().set_title('Humidity Deviation Between Sensors (%RH)')
    plt.show()


def get_tempData():
    ambient = pd.read_csv(path1, sep='\t')
    reliability = pd.read_csv(path2, sep='\t')
    gryffindor = pd.read_csv(path3, sep='\t')
    HP = pd.read_csv(path4, sep='\t')
    HP2 = pd.read_csv(path5, sep='\t')
    HP3 = pd.read_csv(path6, sep='\t')

    ambientTemp = ambient.iloc[:, [1]]
    ambientHum = ambient.iloc[:, [2]]
    reliabilityTemp = reliability.iloc[:, [1]]
    reliabilityHum = reliability.iloc[:, [2]]
    gryffindorTemp = gryffindor.iloc[:, [1]]
    gryffindorHum = gryffindor.iloc[:, [2]]
    HPTemp, HPTemp2, HPTemp3 = HP.iloc[:, [1]], HP2.iloc[:, [1]], HP3.iloc[:, [1]]
    HPHum, HPHum2, HPHum3 = HP.iloc[:, [2]], HP2.iloc[:, [2]], HP3.iloc[:, [2]]

    time = reliability.iloc[:, [0]]
    datapoints = len(time)

    ##################################################### Arrange HP data with padded zeros for appropriate visualization

    zeros = datapoints - len(HPTemp)
    ZeroCol = [0] * zeros

    HPTemp, HPTemp2, HPTemp3 = HPTemp.to_numpy(), HPTemp2.to_numpy(), HPTemp3.to_numpy()
    HPTemp, HPTemp2, HPTemp3 = HPTemp[:, 0], HPTemp2[:, 0], HPTemp3[:, 0]
    HPTemp, HPTemp2, HPTemp3 = np.concatenate((ZeroCol, HPTemp)), np.concatenate((ZeroCol, HPTemp2)), np.concatenate(
        (ZeroCol, HPTemp3))  # temps + zeros
    HPTemp, HPTemp2, HPTemp3 = pd.DataFrame(HPTemp), pd.DataFrame(HPTemp2), pd.DataFrame(HPTemp3)

    HPHum, HPHum2, HPHum3 = HPHum.to_numpy(), HPHum2.to_numpy(), HPHum3.to_numpy()
    HPHum, HPHum2, HPHum3 = HPHum[:, 0], HPHum2[:, 0], HPHum3[:, 0]
    HPHum, HPHum2, HPHum3 = np.concatenate((ZeroCol, HPHum)), np.concatenate((ZeroCol, HPHum2)), np.concatenate(
        (ZeroCol, HPHum3))  # temps + zeros
    HPHum, HPHum2, HPHum3 = pd.DataFrame(HPHum), pd.DataFrame(HPHum2), pd.DataFrame(HPHum3)

    ##################################################### make datelist
    date1 = '07/07/2021'
    finaldate = date.today().strftime("%m/%d")

    datelist = [datetime.strftime('%m/%d') for datetime in
                pd.date_range(date1, periods=int(np.ceil(datapoints / 288))).tolist()]

    ######################################################

    temps = pd.concat([time, ambientTemp, reliabilityTemp, gryffindorTemp, HPTemp, HPTemp2, HPTemp3],
                      axis=1)  # , HPTemp, HPTemp2, HPTemp3
    hums = pd.concat([time, ambientHum, reliabilityHum, gryffindorHum, HPHum, HPHum2, HPHum3], axis=1)

    temps.columns = ['time', 'Ambient', 'Reliability', 'Gryffindor', 'HPAmbient', 'HPLeak O',
                     'HPLeak I']  # , 'HP ambient', 'HP Leakage outside', 'HP Leakage inside'
    hums.columns = ['time', 'Ambient', 'Reliability', 'Gryffindor', 'HPAmbient', 'HPLeak O',
                    'HPLeak I']  # Set column labelling

    return temps


# Press the green button in the gutter to run the script.
if __name__ == '__main__':

    #temp_stats()
    plot_temps()
    #thermData()



