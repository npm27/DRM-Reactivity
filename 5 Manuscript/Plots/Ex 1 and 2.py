####Experiment 2-4####
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

dat = pd.read_csv("Ex 1 and 2.csv")

#make the 95% confidence intervals
dat['diff'] = dat['Upper'].sub(dat['Lower']) #get the length of the bars
dat['diff2'] = dat['diff'].div(2) #length from line to point

##set up the initial plot
fig = plt.figure()
fig.set_size_inches(22,26)

ax1 = fig.add_subplot(2, 1, 1)
ax2 = fig.add_subplot(2, 1, 2)

#fig.tight_layout()
fig.subplots_adjust(top = .91)
                   
plt.subplots_adjust(hspace = 0.25)

fig.suptitle('Experiments 1 and 2: Categorized vs. Uncategorized Lists', fontsize = 42, fontweight = 'bold')

####Subset by Experiment####
ex2 = dat[dat['Experiment'] == 1]
ex3 = dat[dat['Experiment'] == 2]

####Experiment 1####
#subset by task
#subset by task
i1 = ex2[ex2['Task'] == 'IJOL']
g1 = ex2[ex2['Task'] == 'GJOL']
r1 = ex2[ex2['Task'] == 'Read']

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
ax1.set_title('Experiment 1: Free-Recall', fontsize = 40, fontweight = 'bold')
ax1.set_ylabel('Mean Proportion Recall', fontsize = 36, fontweight = 'bold')
ax1.set_xlabel('List Type', fontsize = 36, fontweight = 'bold')
#ax1.xaxis.labelpad = 0
ax1.set_xticks(ind + .15)
ax1.set_xticklabels(('Categorized', 'Uncategorized'), fontsize = 32)
ax1.tick_params(axis='x', which='major', pad=15)
ax1.tick_params(axis="y", labelsize=32)
ax1.legend(fontsize = 32)
ax1.set_ylim([0,1])

####Experiment 2#####
i2 = ex3[ex3['Task'] == 'IJOL']
g2 = ex3[ex3['Task'] == 'GJOL']
r2 = ex3[ex3['Task'] == 'Read']

#get all the things to plug into the plots
#separate out averages and conf interval
i2_average = i2['Average']
g2_average = g2['Average']
r2_average = r2['Average']

i2_conf = i2['diff2']
g2_conf = g2['diff2']
r2_conf = r2['diff2']

ind = np.arange(len(i2_average))  # the x locations for the groups
width = 0.30 #bar width 

rects4 = ax2.bar(ind - width/2, i2_average, width, yerr = i2_conf, capsize = 3, color = 'white', edgecolor = 'k',
                label ='Item JOL', align = "center")

rects5 = ax2.bar(ind + width/2, g2_average, width, yerr = g2_conf, capsize = 3, color = 'silver', edgecolor = 'k',
                label = 'Global JOL', align = "center")

rects6 = ax2.bar(ind + width + .15, r2_average, width, yerr = r2_conf, capsize = 3, color = 'dimgrey', edgecolor = 'k',
                label = 'No-JOL', align = "center")

#Make the plot spiffy
ax2.set_title('Experiment 2: Recognition Testing', fontsize = 40, fontweight = 'bold')
ax2.set_ylabel('Mean Prop. "old" Responses', fontsize = 36, fontweight = 'bold')
ax2.set_xlabel('List Type', fontsize = 36, fontweight = 'bold')
#ax1.xaxis.labelpad = 0
ax2.set_xticks(ind + .15)
ax2.set_xticklabels(('Categorized', 'Uncategorized', 'New'), fontsize = 32)
ax2.tick_params(axis='x', which='major', pad=15)
ax2.tick_params(axis="y", labelsize=32)
ax2.legend(fontsize = 32)
ax2.set_ylim([0,1])

#fig.savefig('EX1_2_chart.png', dip = 10000)