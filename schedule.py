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
        MTT = 1
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
        total_units = 1
    else:
        total_units = 2 #len(unit_type)
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

    # Our range of active unit indices
    # We'll use this to coerce the unit 0 to correspond to
    # a null unit, or unassigned value.
    units = range(1,total_units + 1)
    unassigned = 0

    ## unit_assigned(m,w) #  decision variable determining whether training
    ## team m, during week w of the schedule, results in a trained unit.
    ##unit 0 is implicitly the null unit (e.g. no trainining), where
    ##units 1..total_units indicate a unit index.

    #This lets us say "we want to create family of variables, across the
    #index m_u_w, with similar properties (in this case they're binary)
    unit_assigned = LpVariable.dicts('unit_assigned',utils.product(ms,ws),
                              lowBound = 0.0, upBound = total_units + 1)

    #add a var to track unit training

    ##introduce a binary decision variable to decide whether
    ##unit u was trained by week w:
    trained = LpVariable.dicts('trained',utils.product(units,ws),
                                lowBound = 0.0, upBound = 1, cat=LpInteger)
    # these are all assignable units, vs. the unassigned unit.
    # if the unit is trained, it's non-zero index must be =
    # to sum of the unit_assigned(m,w) for any concurrent training
    # assignment.  Since unit_assigned(m,w) can take on 0, this allows
    # slack, where the only non-zero number must correspond with the
    # binary gate indicated by trained(u,w).  Thus, unit_assigned
    # may only take on non-zero values for assignable unit indices,
    # when a trainable unit it chosen for training via trained(u,w).
    for w in ws:
        sched += lpSum((trained[u,w] * u) for u in units) == \
                 lpSum(unit_assigned[m,w] for m in ms)

    #We have a linear variable that only takes on binary values
    #of 0 or 1 to indicate the assignment of mtt m, to unit u,
    #at time w.  Since unit_assigned can only take on non-zero
    #values when trained(u,w) is 1 for the unit, this leads to
    #a singular value for unit_assigned which is identical
    #to the index of u in trained(u,w).  If the unit is not
    #trained, then unit_assigned will be 0, or unassigned.
    #So we can coerce our implicitly integral value for
    #unit_assigned by dividing the the related unit index constant.
    #Summing across all assigned indices (either 0 or a unit
    #index defined by train(u,w)), we then have 1 possibility
    #for non-zero assignment.  Dividing by the unit index
    #projects the index onto the set {0,1}, giving us an implicit
    #integer variable.
    assign = LpVariable.dicts('assign',utils.product(ms,units,ws),
                              lowBound = 0.0, upBound = 1)
    for (m,w) in product(ms,ws):
        #pulp makes us do this, otherwise it'll complain if we inline
        #the division...
        mult = {u:1 / u for u in units}
        sched += assign[m,u,w] == lpSum((unit_assigned[m,w] * mult[u])
                                        for u in units)


    #Objective function: z = assigncost = sum(m,u,w)assign(m,u,w)
    #min z, e.g. minimize the total number of assignments
    #For now, we reflect no higher order notion of cost within the parameters.
    #One could easily envision certain combinations being more desirable,
    #hence the inclusion of a weight parameter
    assigncost = LpVariable('assigncost')
    sched += assigncost == lpSum(assign[(m,u,w)]*1.0
        for (m,u,w) in assign.keys())

    #helper variables:
    #mtt training events per week
    #events(m,w) = sum(u)assign(m,u,w) forall m in ms,w in ws, u in us
    events = LpVariable.dicts("events",utils.product(ms,ws))
    for (m,w) in events.keys():
        sched += events[m,w]  == lpSum((assign[m,u,w] for u in units))

    #only one training event allowed per week
    for (m,w) in events.keys():
        sched += events[m,w] <= 1

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

    wait = LpVariable.dicts("wait",utils.product(units,ws),lowBound = 0.0)
    for u in units:
        #our unit_type index is 0-based, we need to offset by 1 in the
        #formulation.
        interval = interval_type[unit_type[u-1]]
        for w in utils.butlast(ws):
            sched += wait[u,w + 1] == wait[u,w] + 1 - trained[u,w]*(interval + 1)

    #we can train a unit more frequently, but not lapse in training.
    #Wait times cannot exceed mandated training intervals.
    #note: we can add a deviation goal to this later if we want
    #to or need to relax this.  Alternately, include in objective...

    for (u,w) in wait.keys():
        #we need to offset our unit index, since unit_type is 0 based.
        sched += wait[u,w] <= interval_type[unit_type[u-1]]

    #goal is to minimize the total number of assignments we need.
    #This should induce maximal wait times as well.
    sched.setObjective(assigncost)
    def getsolution():
        res = {}
        for (m,u,w) in assign.keys():
            if value(assign[m,u,w]) > 0.0 :
                res[m,u,w] = value(assign[m,u,w])
        return {'assigned':res, #dictvals(assign),
                'unit_assigned':dictvals(unit_assigned),
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
