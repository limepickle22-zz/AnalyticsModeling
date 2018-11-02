##R Code 12.2##
> library(FrF2)
> home_factors = c('Large Yard', 'Solar Roof', 'Granite Countertops', 'Extra Bedroom', 'Upgraded Shower', 'Large Master 
+ Closet', 'Wood Floors', 'Gas Stove', 'Kitchen Vent', 'Patio')
> FrF2(16, factor.names = home_factors)

##Python Code 13.2##
import random
import numpy as np
import simpy


randomSeed = 101

numSecurity = 30 # Number of servers
securityTime = np.random.exponential(.75)    # Minutes it takes to service taken randomly from exponential distribution
numCheck = 30 # Number of check queues
checkTime = np.random.uniform(low = .5, high = 1) # Minutes to complete check taken randomly from uniform distribution
passengerIntervalTime = np.random.exponential(.02)    # Time between passenger arrival taken randomly from exponential distribution
simulationTime = 100    # Simulation time in minutes
waitTime = []

class Security(object):
    def __init__(self, env, num_servers, serv_time):
        self.env = env
        self.machine = simpy.Resource(env, num_servers)
        self.secTime = serv_time

    def service(self):
        yield self.env.timeout(securityTime)

class CheckQueue(object):
    def __init__(self, env, num_check, check_time):
        self.env = env
        self.machine = simpy.Resource(env, num_check)
        self.secTime = check_time

    def service(self):
        yield self.env.timeout(checkTime)

def passenger(env, name, security, check_queue):
    timeArrive = env.now
    print('%s arrives at the airport at %.2f' % (name, env.now))
    with security.machine.request() as request:
        yield request

        print('%s enters security at %.2f' % (name, env.now))
        yield env.process(security.service())

    with check_queue.machine.request() as request:
        yield request

        print('%s enters check queue at %.2f' % (name, env.now))
        yield env.process(check_queue.service())
        print('%s proceeds to gate at %.2f' % (name, env.now) + ' minutes')
        waitTime.append(env.now - timeArrive)
    
    print(sum(waitTime)/len(waitTime)) #Average Wait Time


def setup(env, num_security, security_time, num_check, check_time, t_inter):
    # Create the airport/security/check queue
    security = Security(env, num_security, security_time)
    check = CheckQueue(env, num_check, check_time)

    # Create 5 initial passengers
    for i in range(5):
        env.process(passenger(env, 'Passenger %d' % i, security, check))

    # Create more passengers while the simulation is running
    while True:
        yield env.timeout(t_inter)
        i += 1
        env.process(passenger(env, 'Passenger %d' % i, security, check))


# Setup and start the simulation
print('Airport')
random.seed(randomSeed)  # This helps reproducing the results

# Create an environment and start the setup process
env = simpy.Environment()
env.process(setup(env, numSecurity, securityTime, numCheck, checkTime, passengerIntervalTime))

# Execute!
env.run(until=simulationTime)