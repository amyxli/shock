# what we need
# axl 13/03/22
# last edited axl 21/03/22

All actions here are performed by the macro Int5s_extract.

- baseline for EACH TRIAL - average over 5s interval: 4s before "proceed" (ie when proceed is clicked) + 1s after "proceed"
1a) Multiple add to datapad > 4s before "proceed"; this averages over the 4s period
1b) Multiple add to datapad > 1s after "proceed"; this averages over the 1s period

-> deriving average of 5s period over the 4s and 1s averages

Let a be the average over the 4s period before "proceed"; 
Let b be the average over the 1s period after "proceed".
We would like to derive M_base, the average of 5s period over these 2 periods.

M_base = a*(4/5)+b*(1/5)

- 5s following choice (FON or KIS)
	- ideally want EVERY 5s following choice x 4 (for total of 20s)
2a) Multiple add to datapad > 5s after "FON"
2b) Multiple add to datapad > 10s after "FON"
2c) Multiple add to datapad > 15s after "FON"
2d) Multiple add to datapad > 20s after "FON"

3a) Multiple add to datapad > 5s after "KIS"
3b) Multiple add to datapad > 10s after "KIS"
3c) Multiple add to datapad > 15s after "KIS"
3d) Multiple add to datapad > 20s after "KIS"

- we want to identify whether it is a FON_shock (ie 100% shock prob), FON_nothing (ie 0% shock prob), or KIS (ie 50% shock prob)
4a) Multiple add to datapad > 5s after "shock"
4b) Multiple add to datapad > 5s after "nothing"

-> deriving averages of 5s sub-intervals from 5s, 10s, 15s, and 20s averages

t_0= FON
   |5s   |5s   |5s   |5s   |
   |-----|-----|-----|-----|

Let t_0 be when FON/KIS choice is made.
Let:
a be the average microsiemens from t_(0 to 5s), 
b be the average from t_(0 to 10s), 
c be the average from t_(0 to 15s), and 
d be the average from t_(0 to 20s).
(Note that a, b, c, d are known; can be obtained from labchart.)

Let the desired averages to be derived be:
x_1, the average across the 1st 5s period after t_0 (i.e., t_(0 to 5s));
x_2, the average across the 2nd 5s period after t_0 (i.e., t_(5 to 10s))
x_3, the average across the 3rd 5s period after t_0 (i.e., t_(10 to 15s)); and
x_4, the average across the 4th 5s period after t_0 (i.e., t_(15 to 20s)).

Then,
x_1 = a
(x_1 + x_2)/2 = b
(x_1 + x_2 + x_3)/3 = c
(x_1 + x_2 + x_3 + x_4)/4 = d

Via substitution and making x_1,..., x_4 the subjects of each of the 4 equations respectively, we get

x_1 = a
x_2 = 2b - a
x_3 = 3c - 2b
x_4 = 4d - 3c
