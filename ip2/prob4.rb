# Problem 4: Simulation in Ruby

# Suppose you are in charge of a large website. Based on how many
# requests per second your site usually gets, you want to provision
# enough servers to handle the requests. Your website uses a load
# balancer to decide which webserver should process each request. If a
# request arrives at the load balancer and and no server is available,
# the request will wait in line at the load balancer until a server
# becomes available. On the one hand, you want requests to experience
# as little delay as possible. On the other hand, you want to save
# money by not having more servers than you need.
#
# In this kind of situation, it would be useful to know the answers to
# questions like "If I have N servers and an average of R requests
# arrive per second, each of which takes an average of S seconds
# to process, how long on average will a request wait in line at the
# load balancer?" Such questions are expensive to answer by
# experimenting in the real world, but they can be answered through
# simulation.
#
# In this problem, you will write a simulator in Ruby to answer
# questions like the one above. The simulator will keep track of each
# server, each request, including those in the queue at the load
# balancer, and so on. The simulator will generate new requests
# according to the given rate. We can answer questions like the one
# above by appropriately configuring the simulator, letting it run for
# a while, and computing the average wait time.
#
# The simulator will be structured around *events* that occur at a
# particular time. As the system evolves, events occur, which cause
# the state of the system to updated and new events to be scheduled in
# the future.
#
# We will follow the PL-person approach by first building a *library*
# for simulators like this, and then getting what we need by writing a
# short program on top of the library.
#
# The library consists of two classes: Simulator and ResourcePool. The
# basic implementation of the Simulator class is provided to you
# below. Only a skeleton of the ResourcePool class is provided, so you
# will have to implement it.
#
# The main job of the Simulator class is to keep track of and execute
# events. Internally, a Simulator maintains an array of events, sorted
# in increasing order by time. Each event contains some code to
# execute when that event occurs. The main method of the simulation is
# the `event_loop` method, which continually pulls events off the
# front of the list, and executes the corresponding code. There are
# also methods for scheduling events. Since the code inside an event
# may itself schedule events, the event loop may run for a long time,
# even for ever.
class Simulator
    attr_reader :now

  def initialize
    @now = 0.0  # current time in simulated world
    @events = SortedArrayPriorityQueue.new  # list of events
  end

  # Schedule an event ev at time t.
  #
  # Uses the helper class Event (see below) to package up the
  # "underlying" event ev and the time at which it should execute.
  def schedule_absolute(ev, t)
    @events.enqueue(Event.new(ev, t))
    self
  end

  # Schedule an event ev at time now + delay. Most events will use
  # this method rather than schedule_absolute.
  def schedule(ev, delay=0.0)
    schedule_absolute(ev, now + delay)
  end

  # For debugging, it is helpful to print every event as it is
  # executed, but our simulations will execute a *lot* of events, so
  # it's convenient to be able to turn the messages off.
  Debug = true

  def event_loop
    until done?
      # get the next event
      ev = @events.dequeue
      puts ev if Debug

      # move time forward to the time of the event
      assert(@now <= ev.time, "time moves forward")
      @now = ev.time

      # execute the event, passing the simulator to allow the event to
      # schedule further events
      ev.execute(self)
    end
  end

  # The default stopping condition is just when there are no events
  # left. Subclasses often want to override this to stop after
  # processing a certain number of events, for example.
  def done?
    @events.empty?
  end
end

# That's all there is to the Simulator. It's simple, but it's amazing
# what you can build on top of it.
#
# As a silly example, here is a simulation of a counter-like
# system. Things randomly arrive at a certain rate, and the system
# keeps track of the total number of things that have ever arrived.
#
# You are not required to read or understand this part of the code if
# you don't want to (in that case, skip down to "Problem 4a" below),
# but it may help you understand what's going on with the simulator.
#
# All simulations start by subclassing Simulator. The subclass will
# keep track of the state of the system. In the counting example, the
# state consists of the current count, which is stored in the instance
# variable @count. We also create a public method to increment the
# count, which will be used by the event classes. Finally, we override
# the `done?` method, so that the simulation will terminate when the
# count reaches 100.

class CounterSimulator < Simulator
  attr_reader :count

  def initialize
    super()
    @count = 0
  end

  def increment_count
    @count += 1
  end

  def done?
    super() or count >= 100
  end
end

# Most simulations will also define one or more helper classes to
# model various parts of the system. In the counting example, we will
# use a class Countee to represent the things being counted.
#
# The Countee class is typical of "active" objects in simulations.
# Here, "active" just means that the object takes part in the
# simulation by being scheduled at various times. It will thus
# implement an `execute` method to describe its effect on the
# simulation.
#
# In general, active objects will want to have different effects based
# on their internal state. Although in the simple counting example we
# don't really need to keep track of any state, we do so anyways just
# to show how it's done.

class Countee
  def initialize
    @state = :start
  end

  def execute(sim)
    case @state
    when :start
      @state = :end
      sim.increment_count
    when :end
      raise "Countee executed from end state!"
    else
      raise "Countee executed from invalid state #{@state}"
    end
  end
end

# When a countee is executed from its starting state, it increments
# the counter in the simulator and sets its state to the ending
# state. If it is ever executed from any other state, an error is
# raised. (We recommend you follow this convention of raising errors
# when your active objects are executed from unexpected states, as it
# helps with debugging.)
#
# All that's left is to generate new Countee's at randomly spaced
# times. We'll do this as another active object, called
# CounteeGenerator.  In its execute method, it schedules new events,
# including new Countees and itself!

class CounteeGenerator
  def execute(sim)
    # Generate a Countee, and schedule it (for right now!)
    sim.schedule(Countee.new)

    # Schedule the generator itself for some time in the future, so
    # that it can generate more Countees.
    #
    # The time is selected using the provided method exponential_rand.
    # This method generates a random non-negative number according to
    # the so-called exponential distribution, which commonly used in
    # simulations for the time between a sequence of repeated
    # events. The argument to exponential_rand is the average time
    # between events.
    sim.schedule(self, exponential_rand(5.0))
  end
end

# Now here is a top-level method to actually execute the simulation
# from start to finish. After the simulation ends, this method prints
# a brief report including how long it took to count to 100.
def run_counter_simulation
  sim = CounterSimulator.new
  sim.schedule(CounteeGenerator.new)
  sim.event_loop
  puts "Simulation finished. Current time is #{sim.now}. Counter is #{sim.count}."
end

# Run this method a few times. Since the average time between Countees
# is 5.0, and it counts to 100, you should see total times around 500.
# Since there is randomness involved, you will not get exactly 500,
# but something near 500. If you run the simulation over and over, you
# should gett 500 on average.


# Problem 4a: Implementing ResourcePool

# Before we finally get around to implementing the web server
# simulator, we need a way to model resources like the server. What
# makes something a resource (as opposed to an active object, for
# example) is that typically there are a fixed, limited number of
# them, and they can only be used by one active object at a time. The
# set of all resources of a particular kind will be modeled by a
# single object, the "pool" of resources.
#
# In the web server example, servers will be resources. Requests will
# be active objects that need a server resource in order to be
# served. In our simple model, we assume a server can only serve a
# single request at a time.
#
# The interface to a ResourcePool is as follows:
#
#   - A pool is constructed by giving the capacity of the pool, and
#     the relevant simulator object. The simulator is needed because
#     the pool needs to be able to schedule events.
#
#   - An active object can request a resource from the pool by calling
#     the `acquire` method, which takes the active object making the
#     request as an argument. If a resource is available now, then the
#     `acquire` method immediately schedules the active object for
#     execution on the event loop by calling the `schedule` method on
#     the simulator. Otherwise, if no resources are currently
#     available, the acquire method stores the active object so that
#     it can be scheduled later, when resources become available.
#
#   - An active object can give a resource back to the pool when it is
#     done with it by calling the `release` method. The release method
#     is also responsible for scheduling any active objects that are
#     waiting for resources, being careful not to schedule more active
#     objects than there are resources available.

class ResourcePool
    attr_reader :simulator, :capacity, :resourceCount, :waitlist
    
  def initialize(simulator, capacity)
    @simulator = simulator
    @capacity = capacity
    @waitlist = []
    @resourceCount = 0
  end

  def acquire(active_obj)
      if((@capacity - @resourceCount) > 0)
          @simulator.schedule(active_obj)
          (@resourceCount += 1)
          else
          @waitlist.push(active_obj)
          
      end
  end

  def release
      if(@waitlist.size > 0)
          @simulator.schedule(@waitlist.shift)
      else
          (@resourceCount -= 1)
      end
  end
      
  
end


# Problem 4b: Implementing the server simulator

# We now have all the pieces to simulate the web servers.

# Start by filling out the following subclass of Simulator with the
# additional state required for the server simulation. The main piece
# of state will be some sort of resource representing the servers. You
# will also want to override done? to stop at some point. During
# debugging, it can be useful to simulate a fixed number of requests
# or a fixed number of events.

class ServerSimulator < Simulator
    attr_reader :sim, :pool, :n_servers, :runningResourceCount,:waitLengthArr, :waitTimeArr, :serveTimeArr
    
    def initialize(n_servers)
        super()
        @pool = ResourcePool.new(self, n_servers)
        @cap = n_servers
        @runningResourceCount = @pool.resourceCount
        @waitLengthArr = []
        @waitTimeArr = []
        @serveTimeArr = []
        
    end
    
    def acquire(obj)
        @pool.acquire(obj)
        @runningResourceCount = @runningResourceCount + 1
    end
    
    def release
        @pool.release
        @runningResourceCount = @runningResourceCount
    end
    
    def done?
        super() or @runningResourceCount >= 4000
    end

end

# Now implement the WebRequest active object. As an active object, it
# should implement an execute method that models how a request evolves
# in the system. When a request firsts executes, it should try to
# acquire a server resource from the simulator. Then, when it is next
# executed, it will have been granted the server resource, at which
# point service has begun; it should schedule itself for later
# according to a randomly generated delay, drawn from the exponential
# distribution at a rate given in the initialize method. Finally, when
# it is executed the last time, service will be complete, so it should
# release the resource.

class WebRequest
    attr_reader :rate, :waitLength, :waitTime, :serveTime
    
    def initialize(rate)
        @state = :notRan
        @rate = rate
        @time1 = 0;
        @time2 = 0;
        @time3 = 0;
    end
    
    def execute(sim)
        case @state
            when :notRan
                @state = :ranOnce
                sim.waitLengthArr.push(sim.pool.waitlist.size)
                @time1 = sim.now
                sim.acquire(self)
            when :ranOnce
                @state = :executedLast
                @time2 = sim.now
                sim.waitTimeArr.push(@time2 - @time1)
                sim.schedule(self, @rate)
            when :executedLast
                @time3 = sim.now
                sim.serveTimeArr.push(@time3 - @time2)
                sim.release
            else
                raise "Request executed from invalid state #{@state}"
        end
    end

end

# Now implement a class to generate requests, similar to
# CounteeGenerator.  The times between requests should be drawn from
# an exponential distribution at a rate given in the initialize
# method.  (Note: this rate is separate from the rate used inside of a
# WebRequest to decide how long it will take to serve the request.
# Since WebRequestGenerator needs to construct new WebRequests, which
# requires passing the service rate, the WebRequestGenerator
# initialize method will actually need to take *two* rates: one for
# the average time between requests, and one for the average time to
# serve requests.)

class WebRequestGenerator
    attr_reader :avg_time_between_requests, :avg_time_to_serve_request
    
    def initialize(avg_time_between_requests, avg_time_to_serve_request)
        super()
        @avg_time_between_requests = avg_time_between_requests
        @avg_time_to_serve_request = avg_time_to_serve_request
    end
    
    def execute(sim)
        
        sim.schedule(WebRequest.new(exponential_rand(@avg_time_to_serve_request)))
       
        sim.schedule(self,  exponential_rand(@avg_time_between_requests))
        
    end

end

# Implement a method like run_counter_simulation to execute a
# simulation with a given number of servers, a given average time
# between requests, and a given average time to serve a request. For
# now, just execute the simulation, don't worry about printing any
# report. For debugging purposes, you may want to modify the
# Simulator::Debug flag to be true. Ensure that your simulation is
# behaving as expected by running a short (say, 100 events) simulation
# and reading through the debug output.
def run_server_simulation(n_servers, avg_time_between_requests, avg_time_to_serve_request)
    sim = ServerSimulator.new(n_servers)
    #pool = ResourcePool.new(sim, n_servers)
    sim.schedule(WebRequestGenerator.new(avg_time_between_requests, avg_time_to_serve_request))
    sim.event_loop
    
    
    
    #Statistics
    
    puts "======================================================="
    puts "Server Simulation finished. Current time is #{sim.now}."
    
    #Print our statistics!
    puts "Statistics for this Run:"
    puts "Average Number of Other Requests Waiting: #{((sim.waitLengthArr.inject(:+)).to_f)/(sim.waitLengthArr.size)}"
     puts "Average Time Spent Waiting: #{((sim.waitTimeArr.inject(:+)).to_f)/(sim.waitTimeArr.size)}"
      puts "Average Number of Other Requests Waiting: #{((sim.serveTimeArr.inject(:+)).to_f)/(sim.serveTimeArr.size)}"
    
end

# Problem 4c: Measure average waiting times in various scenarios

# Now, modify your simulator to keep track of various statistics about
# the execution. Then print these statistics in your
# run_server_simulation method. It is simplest to simply record these
# numbers in the simulator in a big list as you go, and then do
# statistical analysis at the end in the run_server_simulation method.
#
# For each request, you should track the following:
#
#   - how many other requests were waiting to be served when this
#     request first tried to acquire a server
#
#   - how long did the request spend in the queue
#
#   - how long did the request take to get served after it acquired a
#     server resource
#
# Your run_server_simulation should print a report describing the
# average of each of the above attributes over all requests processed
# during the simulation.
#
# Consider the following scenarios. For each scenario, run a
# simulation to determine the average time a request spends in the
# queue. Each scenario is given by three numbers: the number of
# servers N, the average time between requests R, and the average
# service time S.
#
#   - N = 1, R = 1.0, S = 0.5
#   - N = 2, R = 1.0, S = 0.5
#   - N = 1, R = 1.0, S = 1.0
#   - N = 2, R = 1.0, S = 1.0
#
# For each scenario, give the averages reported by your simulator on a
# single run of several thousand events (the exact stopping criterion
# doesn't matter).
##################################################################
# Average Wait Time Testing Below (All tests ran on 4000 events):

# - N = 1, R = 1.0, S = 0.5 : (50% load)
#       Test 1: 0.47078
#       Test 2: 0.50344
#       Test 3: 0.53590

# - N = 2, R = 1.0, S = 0.5 : (25% load)
#       Test 1: 0.03854
#       Test 2: 0.03082
#       Test 3: 0.03367

# - N = 1, R = 1.0, S = 1.0 : (100% load)
#       Test 1: 18.3499
#       Test 2: 23.4295
#       Test 3: 15.9687

# - N = 2, R = 1.0, S = 1.0 : (50% load)
#       Test 1: 0.31089
#       Test 2: 0.33252
#       Test 3: 0.32734

###################################################################

# One way to compare these scenarios is using the concept of "load",
# which is defined to be the ratio between the theoretical maximum
# rate of service and the given rate. The first scenario has 50% load,
# because the theoretical maximum service rate is two requests per
# unit time, (the single server takes half a time unit to process a
# request), and the given rate is only one request per unit time (the
# requests come in once per time unit on average). The second scenario
# has 25% load because we added an extra server but didn't change
# anything else. The third scenario has 100% load. The fourth scenario
# also has 50% load.
#
# Using the results of your simulation, what can you say about the
# relationship between load and the average time a request spends in
# the queue? Pay special attention to the first and fourth scenarios,
# which have the same load. Do they have the same average waiting
# time?
#
# After seeing the results for my tests on my server simulator, the
# relationship between server load percentage and the amount of time
# a request will wait in the queue is very clear. In my first scenario
# at 50% load, the majority of the waiting times were around 0.4 - 0.5.
# The second scenario had significantly less average waiting times due
# to the server only being at 25% load (0.030 - 0.040). In the third test
# there was a very big margin, as our server ran at 100% load and resulted
# in an average waiting time per request to be around 15 - 25! This shows
# the large effect a server at full load can show in terms of a request
# waiting to be served. The last scenario was also at 50% load and resulted
# in an average wait time over around 0.30 to 0.33. Comparing the first and
# last scenarios, we can see that both of them were ran at a 50% load and both
# showed really similar results despite having a different number of servers.
# Due to the first scenario having only 1 server but a lower service time,
# it ended up displaying similar results to the 4th scenario, where there were
# 2 servers but a doubled service time. The two scenarios showed results that
# displayed an average waiting time for a request to be within 0.1 of eachother
# (0.4 - 0.5 for scenario 1, and 0.30 - 0.33 for scenario 4). In conclusion, I
# think it is safe to say that the relationship between the amount of servers
# and the average time a request will wait in the queue ultimately depends on the
# external factor of the service time. The two scenarios (1 and 4) had a different
# number of servers but produced similar results due to their varying service time.
# Ultimately, I would say the higher the service time, the higher the number of servers
# you will need to maintain the same average waiting time as a scenario with a
# smaller amount of servers and a lower service time.





# Additional helper code below. Ignore unless you're curious.

class SortedArrayPriorityQueue
  def initialize
    @data = []
  end

  def enqueue(item)
    i = @data.find_index { |x| x.priority > item.priority }
    i = (i or -1)
    @data.insert(i, item)
    self
  end

  def dequeue
    @data.shift
  end

  def empty?
    @data.empty?
  end

  def size
    @data.size
  end
end

# Convenient wrapper class so that objects in the simulation do not
# need to implement the priority method themselves, or keep track of
# the correct simulation time.
class Event
  attr_reader :data, :time

  def initialize(data, time)
    @data = data
    @time = time
  end

  def priority
    @time
  end

def to_s
    "Event: #{@time} #{@data}"
    end

  # The *args syntax means "accept any number of arguments". This is
  # useful when delegating to a method where you're not sure how many
  # arguments it expects.
  def execute(*args)
    @data.execute(*args)
  end
end

def exponential_rand(mu)
  - Math.log(1 - rand) * mu
end

def assert(b, msg)
  raise "Assertion violation: #{msg}" unless b
end
