#include <stdio.h>
#include <stdlib.h>
#include <math.h>

float frac(float d)
{
int laps,partlap,meters;
float tmeters;
if (d <110)
  return 0;
else
  {
  laps = d/400;
  meters = d - laps*400;
  if (meters <= 50)
        partlap = 0;
  else if(meters <= 150)
        partlap = meters - 50;
  else if (meters <=250)
        partlap = 100;
  else if (meters <=350)
        partlap = 100 + (meters - 250);
  else if (meters <=400)
        partlap = 200;
  tmeters= laps*200 + partlap;
  return (tmeters/d);
  }
}

float purdy(double dist,float tsec)
{
/*
 portugese running table, distance, speed
 Table was from World Record times up to 1936
 They are arbitrarily given a Purdy point of 950
*/

double ptable[]= {40.0,11.000, 50.0,10.9960, 60.0,10.9830, 70.0,10.9620,
        80.0,10.934, 90.0,10.9000,100.0,10.8600,110.0,10.8150,
        120.0,10.765,130.0,10.7110,140.0,10.6540,150.0,10.5940,
        160.0,10.531,170.0,10.4650,180.0,10.3960,200.0,10.2500,
        220.0,10.096,240.0, 9.9350,260.0, 9.7710,280.0, 9.6100,
        300.0, 9.455,320.0, 9.3070,340.0, 9.1660,360.0, 9.0320,
        380.0, 8.905,400.0, 8.7850,450.0, 8.5130,500.0, 8.2790,
        550.0, 8.083,600.0, 7.9210,700.0, 7.6690,800.0, 7.4960,
        900.0,7.32000, 1000.0,7.18933, 1200.0,6.98066, 1500.0,6.75319,
        2000.0,6.50015, 2500.0,6.33424, 3000.0,6.21913, 3500.0,6.13510,
        4000.0,6.07040, 4500.0,6.01822, 5000.0,5.97432, 6000.0,5.90181,
        7000.0,5.84156, 8000.0,5.78889, 9000.0,5.74211,10000.0,5.70050,
       12000.0,5.62944,15000.0,5.54300,20000.0,5.43785,25000.0,5.35842,
       30000.0,5.29298,35000.0,5.23538,40000.0,5.18263,50000.0,5.08615,
        60000.0,4.99762,80000.0,4.83617,100000.0,4.68988,
                                  -1.0,0.0 };
double c1=0.20;
double c2=0.08;
double c3=0.0065;
double v,d3,t3,d1,t1,t950,t;
double a,b,k,ps,d=0.1;
float p;
int i;
/* get time from port. table */
/* find dist in table */
for (i=0; dist > d && d>0;i+=2)
  d=ptable[i];
if (d<1)
 return 0;    /* cant find distance*/
i+=-2;
d3=ptable[i];        /* get distance */
t3= d3/ptable[i+1];    /* get time */
d1=ptable[i-2];
t1=d1/ptable[i-1];
printf ("d1/t1=%f/%f, d3/t3=%f/%f\n", d1, t1, d3, t3);
/* use linear interpolation to get time of 950 pt. performance*/
t = t1 + (t3-t1)*(dist-d1)/(d3-d1);
v = dist/t;
/* now add the slow down from start and curves */
t950 = t +c1+c2*v +c3*frac(dist)*v*v;
printf ("t/v/t950 = %f/%f/%f\n", t, v, t950);
/* calc purdy points */
k = 0.0654 - 0.00258*v;
a = 85/k;
b = 1-950/a;
printf ("k/a/b = %f/%f/%f\n", k, a, b);
p = a*(t950/tsec - b);        /* here it is */
return p;
}

int main (int argc, char **argv)
{
  if (argc != 3)
    {
      fprintf (stderr, "usage: %s meters seconds", argv[0]);
      return 1;
    }

  double meters = atof (argv[1]);
  double seconds = atof (argv[2]);

  printf ("purdy points = %f\n", purdy (meters, seconds));

  return 0;
}
