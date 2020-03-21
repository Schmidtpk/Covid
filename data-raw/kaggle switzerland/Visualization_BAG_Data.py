import pandas as pd
import numpy
import sys
from datetime import datetime
from datetime import timedelta
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit


def exponential_func(x, a, b, c):
    return a*numpy.exp(-b*x)+c


def scatterplot(datesincezero, poscasearray):
    plt.scatter(datesincezero, poscasearray)
    plt.xlabel("Date")
    plt.ylabel('# of positive tests (BAG)')


data = pd.read_csv("https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid19_cases_switzerland.csv")
canton = input('Which canton do you want to check? (inputformat: ZH, LU, BE, CH,...) \n')
data = data.filter(items=['Date', canton])  # only keep data for one canton (or whole CH) in dataframe
data.rename(columns={canton: "TotalPosTests1"}, inplace=True)
data = data.dropna()  # remove dates without available data (na)
newdata = data
startrow = 0
for x in data['TotalPosTests1']:
    if x == 0:
        startrow+=1
newdata = newdata.iloc[startrow:]
inhabitantsCH = 8570000
datearray = newdata['Date'].to_numpy()
for x in datearray:
    x = datetime.strptime(x, '%Y-%m-%d')
datetimearray = [datetime.strptime(x, '%Y-%m-%d').date() for x in datearray]
try:
    firstdate = datetimearray[1]
except IndexError:
    sys.exit('No cases in database for {}.'.format(canton))

datesincezero = [(x - firstdate).days + 1 for x in datetimearray]
poscasearray = newdata['TotalPosTests1'].to_numpy().tolist()
scatterplot(datesincezero, poscasearray)
x = datesincezero
y = poscasearray
try:
    popt, pcov = curve_fit(exponential_func, x, y, p0=(1, -1e-6, 1))
    xx = numpy.linspace(0, len(newdata.index), 1000)
    yy = exponential_func(xx, *popt)
    plt.plot(xx, yy, 'g--', label='curve-fit: a=%5.3f, b=%5.3f, c=%5.3f' % tuple(popt))
    exponentialfit = True
except RuntimeError:  # if no exponential fit can be found, do not try to plot it and pass
    exponentialfit = False
    pass
except TypeError:  # if not enough data points are available for curve_fit
    exponentialfit = False
    pass

plt.legend()
plt.title('COVID-19 cases in {}'.format(canton))
plt.xticks(x, datetimearray, rotation=40, ha="right")
plt.tight_layout()  # larger bottom margin for readability of dates
plt.gca().spines['top'].set_visible(False)
plt.gca().spines['right'].set_visible(False)
plt.grid(color='grey', linestyle='-', linewidth=0.25, alpha=0.5)
plt.show()
plt.close()


while exponentialfit:  # only run prediction part if exponential-fit was successful
    try:
        futuredays = input('How many days in the future (Day 0 = {})? \n'.format(datetimearray[-1]))
        postestedindividuals = int(round(exponential_func(int(futuredays)+datesincezero[-1], *popt)))
    except ValueError:  # if the user input is not an integer number
        continue
    if postestedindividuals > inhabitantsCH:
        print('Number is higher than all CH inhabitants:')
        print(str(postestedindividuals) + ' COVID-19 tested individuals in {} ({}) \n'.format(canton, datetimearray[-1]+timedelta(days=int(futuredays))))
    else:
        print(str(postestedindividuals)+' COVID-19 tested individuals in {} ({}) \n'.format(canton, datetimearray[-1]+timedelta(days=int(futuredays))))
