# Simulating the visa process

![](images/forecast%20arrivals%20-%20winter%20surge.png)

The British Government is running three concurrent routes to the UK for people fleeing Ukraine:

1.  Ukraine Family Scheme
2.  Homes for Ukraine (individual sponsorship scheme)
3.  Government 'super sponsor' schemes in Wales and Scotland

Regardless of the scheme, people go through a similar process before arriving in the UK: they apply for a visa; the application gets processed, most often resulting in a visa being issued; then the visa-holder travels to the UK.

The diagram below shows how we've modeled the flow of applications to arrivals, which revolves around two backlogs (green boxes) -- one for applications yet to be processed; and one for issued visas where the visa-holder is yet to arrive -- and the rates of transitioning between these boxes (black arrows).

![](https://docs.google.com/drawings/d/e/2PACX-1vQe4Vk2vEkw0R8hj1SbXIxxhG8oTI0kPaavDnK06okRRddhNxuKGvov-HW__8-0HZiWCs0Hwti0Kqxt/pub?w=1522&h=467)

The simulation is comprised of five core components:

1.  The backlog of visa applications that need to be processed
2.  The weekly rate of visas issued from the application backlog
3.  The backlog of issued visas, where the visa-holder hasn't yet arrived in the UK
4.  The weekly rate of arrivals from the issued visas backlog
5.  The number of new applications submitted

## 1. Visa application backlog

Each week, visas are issued, refused, or withdrawn - which shrinks the application backlog - and new applications are submitted, which adds to the backlog. Based on the additional number of visas issued each week and the size of the application backlog in a given week, we can calculate the proportion of the application backlog that results in a visa being issued.

## 2. Application processing rates

The Family and Sponsorship schemes have similar rates of converting applications into visas each week - 15.5% under the Family Scheme and 16.8% under the Sponsorship scheme. DLUHC hasn't published a lot of historical data for this part of the process, so for simplicity we combine them into a single rate of visas being issued from the backlog of applications.

![](images/simulation/Proportion%20of%20applications%20that%20are%20issued%20a%20visa%20each%20week.png)

Refusal and withdrawal rates for applications have remained low: around 5% for the Family Scheme and 2% of applications to the Sponsorship Scheme. We assume these rates remain constant in the simulation.

## 3. Backlog of issued visas

There is also a backlog of visas that have been issued but the visa holders are yet to arrive in the UK. This is calculated as the number of additional arrivals in a given week divided by the previous week's number of issued visas that were yet to arrive. This assumes people will generally arrive in the UK around a week after their visa has been issued, which may not always happen.

## 4. Arrival rates

The weekly proportion of people arriving in the UK from the visa-issued backlog fluctuates within and between schemes. We fit linear regressions (one per visa scheme) to predict arrival rates and use these predicted rates for future, simulated weeks.

The trend line for the 'super sponsor' schemes is a bit less certain so we just use the Ukraine Family Scheme's predicted arrival rates for the Government schemes.

![](images/simulation/Weekly%20arrivals%20as%20a%20proportion%20of%20the%20backlog%20of%20issued%20visas.png)

## 5. New visa applications

Although the number of new visa applications submitted each week fluctuates, the overall trend has been a downwards on the Family and Sponsorship schemes. For the baseline scenario, we fit linear regressions (one for each scheme) and use the out-of-sample predicted values for the future, simulated weeks.

![](images/simulation/weekly%20applications.png)

Wales and Scotland paused new applications to their 'super sponsor' schemes on 10 June and 13 July, respectively, so we assume there will be no new applicants through that route.

## Simulating arrivals

Now we have all the core components:

- The backlog of issued visas, where the visa-holder hasn't yet arrived in the UK.
- The weekly rate of arrivals from the issued visas backlog.
- The backlog of visa applications that need to be processed.
- The weekly rate of visas issued from the application backlog.
- The rate of new applications submitted.

From these five components, we can simulate the number of new arrivals each week. We seed the simulation with the latest backlogs for each visa scheme taken from the most recently available data [link]. The simulation starts at the most recent week, and we can run it for as many weeks as we want.

The algorithm looks like this:

    Loop over each week of the simulation: 
    ├── Calculate the number of new weekly arrivals under each scheme from last week's backlog of issued visas
    ├── Remove these new arrivals from the issued-visa backlog Calculate the number of new applications to the family and individual sponsorship schemes, based on historical trends
    ├── Remove the (small) proportion of applications that will be refused or withdrawn
    ├── Add the remainder to the backlog of applications
    ├── Convert a proportion of the previous week's applications into issued visas
    ├── Add these new visas to the issued-visas backlog
    └── Remove newly issued visas from the backlog of applications

The code is in [`R/arrivals from Ukraine - trends and forecasts.R`](R/arrivals%20from%20Ukraine%20-%20trends%20and%20forecasts.R)

## Testing the simulation

We can test the simulation by checking how well it predicts already-observed numbers of arrivals. To do so, we set the simulation to start at a week further back in the observed data and run it forward. (You shouldn't set it to start before week 20, since before that there isn't enough historical data to initialise the backlogs in the simulation.)

The simulation nicely predicts the observed data from as far back as week 20: three months in the past, at the time of writing. Observed arrivals are towards the upper end of the predicted range; this is most likely because the simulation uses rates of applications and arrivals that decline faster than the observed decline three months ago.

![](images/forecast%20arrivals%20-%20testing%20the%20simulation.png)
