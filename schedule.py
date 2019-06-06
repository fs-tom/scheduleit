#-------------------------------------------------------------------------------
# Name:        module1
# Purpose:
#
# Author:      tom
#
# Created:     23/05/2019
# Copyright:   (c) tom 2019
# Licence:     <your licence>
#-------------------------------------------------------------------------------

from itertools import product
import utils
import pickle
from pulp import *

def dictvals(d):
    return {k : value(d[k]) for k in d.keys()}

#rough packaging of information we'll use to
#define a MIP problem instance.  We'll refine this
#to closely match the data we pull out of Excel going forward.
def sampleData(small=True):

    ##This is just some lame stuff.
    #total number of MTT teams
    if small == True:
        MTT = 10
    else:
        MTT = 10
    #total weeks
    if small == True:
        WKS = 156
    else:
        WKS = 156
    years = WKS//52
    #mtt teams
    ms = range(MTT)
    #weeks
    ws = range(WKS)
    #unit types
    types = {'cst','dcrf','cre','sr','hrf','cerfp'}

    #mapping of unit->type
    ##this is one cough-syrup addled way to do it
    ##with dictionary comprehensions.
    csts   = {x:'cst' for x in range(0,55)}
    dcrfs  = {x:'dcrf' for x in range(55,61)}
    cres   = {x:'cre' for x in range(61, 65)}
    srs    = {x:'sr' for x in range(65,77)}
    hrfs   = {x:'hrf' for x in range(77,87)}
    cerfps = {x:'cerfp' for x in range(87,104)}

    #assuming we have a table of id->type
    unit_type = utils.merge(csts,dcrfs,cres,srs,hrfs,cerfps)

    #lame total unit count
    if small == True :
        total_units = 10
    else:
        total_units = len(unit_type)
    #range of unit ids...
    us = range(total_units)
    # training interval relative to type, weeks
    interval_type =  {'sr'   :13,
                      'cre'  :52,
                      'dcrf' :52,
                      'cst'  :78,
                      'hrf'  :78,
                      'cerfp':78}
    ys = range(years)

    constant_demands = {'sr':6, 'dcrf':6, 'hrf':10,'cerfp':17}

    static_demands = \
        {(t,y):constant_demands.get(t)
         for t,y in utils.product(types,ys)
         if t in constant_demands}

    variable_demands = {('cre',1):1,('cre',2):4,('cre',3):2}

    #all the yearly demands.
    #yearly demands can be trivially translated to weekly
    #demands...
    yearly_demands = utils.merge(variable_demands, static_demands)

    return {'MTT':MTT,
            'WKS':WKS,
            'ms':ms,
            'ws':ws,
            'types':types,
            'total_units':total_units,
            'us':range(total_units),
            'unit_type':unit_type,
            'interval_type':interval_type,
            'msn_demand':yearly_demands}

#munge our input into a dictionary we can use
def readInputData():
    return sampleData()

###this helps recover our solution in an ordered fashion.

#given a set of parameters, defines an instance of a scheduling model using
#the PuLP modelling functions.  As stated, PuLP will use its built in
#solver.
def buildModel(params):
#asumming we're passed a dictionary of requisite
#parameters, we can unpack it and construct a model
#from the inputs.

#bind the parameters locally for easier reference.
    (MTT,      #scalar, Total number of training teams
    ms,       #numerical range of [0..MTT)
    WKS,      #scalr, total number of weeks to schedule
    ws,       #numerical range of [0..WKS)
    types,    #set of unit types
    total_units, #scalar count of units
    us,         #numerical range [0..total_units)
    unit_type,  #unitid->type
    interval_type,   #unit_type->training-interval
    #all the yearly demands.
    #yearly demands can be trivially translated to weekly
    #demands...
    msn_demand) = utils.get_keys(params,'MTT','ms','WKS','ws','types',
    'total_units','us','unit_type', 'interval_type','msn_demand')

##    #setting up a minimization.
    sched = LpProblem("Sched", LpMinimize)
    #variables

    print(len(ws))
## assign(m,u,w) # binary decision variable determining whether training
## team m is assigned to train unit u, during week w of the schedule.


   #This lets us say "we want to create family of variables, across the
   #index m_u_w, with similar properties (in this case they're binary)
    assign = LpVariable.dicts('assign',utils.product(ms,us,ws),
                              lowBound = 0.0, upBound = 1.0, cat=LpInteger
                              )

    #Objective function: z = assigncost = sum(m,u,w)assign(m,u,w)
    #min z, e.g. minimize the total number of assignments
    #For now, we reflect no higher order notion of cost within the parameters.
    #One could easily envision certain combinations being more desirable,
    #hence the inclusion of a weight parameter
    assigncost = LpVariable('assigncost')
    sched += assigncost == lpSum(assign[(m,u,w)]*1.0
        for (m,u,w) in utils.product(ms,us,ws))

    #helper variables:
    #mtt training events per week
    #events(m,w) = sum(u)assign(m,u,w) forall m in ms,w in ws, u in us
    events = LpVariable.dicts("events",utils.product(ms,ws))
    for (m,w) in events.keys():
        sched += events[m,w]  == lpSum((assign[m,u,w] for u in us))

    #only one training event allowed per week
    for (m,w) in events.keys():
        sched += events[m,w] <= 1

    #add a var to track unit training

    trained = LpVariable.dicts("trained",utils.product(us,ws))
    for (u,w) in trained.keys():
        sched += trained[u,w] == lpSum((assign[m,u,w] for m in ms))

    #unit training credits by interval
    #We account for training intervals by
    #recording wait times as a function of
    #max training intervals and previous
    #wait times.  We introduce the linear
    #decision variable, wait(u,w), which
    #indicates how long unit u has been waiting
    #for training at week w.

    #we may want to add in some minimum frequency too, to ensure
    #we don't allow things like month-month training, which is likely
    #infeasible in practice.  Something like, 1/2 the interval is the
    #lower bound.

    wait = LpVariable.dicts("wait",utils.product(us,ws),lowBound = 0.0)
    for u in us:
        interval = interval_type[unit_type[u]]
        for w in utils.butlast(ws):
            sched += wait[u,w + 1] == wait[u,w] + 1 - trained[u,w]*(interval + 1)

    #we can train a unit more frequently, but not lapse in training.
    #Wait times cannot exceed mandated training intervals.
    #note: we can add a deviation goal to this later if we want
    #to or need to relax this.  Alternately, include in objective...
    for (u,w) in wait.keys():
        sched += wait[u,w] <= interval_type[unit_type[u]]

    #goal is to minimize the total number of assignments we need.
    #This should induce maximal wait times as well.
    sched.setObjective(assigncost)
    def getsolution():
        res = {}
        for (m,u,w) in assign.keys():
            if value(assign[m,u,w]) > 0.0 :
                res[m,u,w] = value(assign[m,u,w])
        return {'assigned':res, #dictvals(assign),
                'trained':dictvals(trained),
                'wait':dictvals(wait),
                'assigncost':value(assigncost)}
    return (sched,getsolution)


###keep track of our last solve, since this can take some time.
lastsolution = None

###Reads external data for parameters necessary to define TMAS, builds the model,
###solves the model, prints results.  Note: since we have Python data structures
###here, it should be really easy to pipe the results into post processing and
###other functions, using all of the Python facilities for operating on data.
def main(run = False):
    global lastsolution
    #should be replaced with IO functions to actual data
    if run:
        params = sampleData(small=False)
        sched,getsolution = buildModel(params)
        print("Solving model....")
        sched.solve()
        print("Status:", LpStatus[sched.status])
        res = getsolution()
        lastsolution = res
        return res
##
##def spit(obj,path = "obj.py"):
##    with open(path, 'wb') as outfile:
##        pickle.dump(obj,outfile)
##
##def load(path= "obj.py"):
##    with open(path, "rb") as infile:
##        return pickle.load(infile)
##
##def writelp():
##    params = inputDataToParams(readInputData())
##    squirm,getsolution = buildModel(params)
##    print("Saving model as SqurimModel.lp....")
##    squirm.writeLP("SquirmModel.lp")
##
##
##def restable(res):
##    return post.asTable(post.getRecords(res))
