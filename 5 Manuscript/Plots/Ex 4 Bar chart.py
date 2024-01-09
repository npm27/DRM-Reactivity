####Experiment 1####
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

dat = pd.read_csv("Ex 4.csv")

#make the 95% confidence intervals
dat['diff'] = dat['Upper'].sub(dat['Lower']) #get the length of the bars
dat['diff2'] = dat['diff'].div(2) #length from line to point

##set up the initial plot
fig = plt.figure()
fig.set_size_inches(16,12)

ax1 = fig.add_subplot(1, 1, 1)

#subset by task
i1 = dat[dat['Task'] == 'IJOL']
g1 = dat[dat['Task'] == 'GJOL']
r1 = dat[dat['Task'] == 'Read']

#get all the things to plug into the plots
#separate out averages and conf interval
i1_average = i1['Average']
g1_average = g1['Average']
r1_average = r1['Average']

i1_conf = i1['diff2']
g1_conf = g1['diff2']
r1_conf = r1['diff2']

ind = np.arange(len(i1_average))  # the x locations for the groups
width = 0.30 #bar width 

rects1 = ax1.bar(ind - width/2, i1_average, width, yerr = i1_conf, capsize = 3, color = 'white', edgecolor = 'k',
                label ='Item JOL', align = "center")

rects2 = ax1.bar(ind + width/2, g1_average, width, yerr = g1_conf, capsize = 3, color = 'silver', edgecolor = 'k',
                label = 'Global JOL', align = "center")

rects3 = ax1.bar(ind + width + .15, r1_average, width, yerr = r1_conf, capsize = 3, color = 'dimgrey', edgecolor = 'k',
                label = 'No-JOL', align = "center")

#Make the plot spiffy
ax1.set_title('Experiment 4: Recognition Testing', fontsize = 40, fontweight = 'bold')
ax1.set_ylabel('Mean Prop. "old" Responses', fontsize = 36, fontweight = 'bold')
ax1.set_xlabel('Item Type', fontsize = 36, fontweight = 'bold')
#ax1.xaxis.labelpad = 0
ax1.set_xticks(ind + .15)
ax1.set_xticklabels(('Presented', 'Critical Lure'), fontsize = 32)
ax1.tick_params(axis="y", labelsize=32)
ax1.legend(fontsize = 32)
ax1.set_ylim([0,1.1])

##save figure
#fig.savefig('EX4_chart.png', dip = 10000)
