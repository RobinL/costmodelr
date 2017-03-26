o![Build Status](https://travis-ci.org/RobinL/costmodelr.svg?branch=master)](https://travis-ci.org/RobinL/costmodelr)
[![Coverage Status](https://img.shields.io/codecov/c/github/RobinL/costmodelr/master.svg)](https://codecov.io/github/RobinL/costmodelr?branch=master)

## Cost modelling in R


This repo contains a framework for cost modelling in R.

The most up to date documentation can be found in the package vignette, which you can access in R by typing `browseVignettes("costmodelr")`

It is based around several ideas:

* A cost model is usually made up of many different types of costs, each one of which will be referred to as a 'line item'.  Examples include the cost of buying capital equipment, costs for a member of staff, recurring licencing fees etc.

* These costs can be grouped in a number of types, each one of which can be modelled in the same way  - ongoing, monthly, one off etc

* It's possible to define a common target data structure - a table - that is the same for all of these costs.  By converting each line item into this tabular data structure and concatenating, the entire cost model can be represented as a single (tidy)[http://vita.had.co.nz/papers/tidy-data.pdf] data frame.  This can then be used as the basis of summary model outputs, such as pivot tables and charts.


### The target data structure

Target data structured is as follows:

```
+------+--------------+----------+----------------+---------------------+
| date | line_item_id | quantity | price_gbp_real | total_cost_gbp_real |
+------+--------------+----------+----------------+---------------------+
|      |              |          |                |                     |
+------+--------------+----------+----------------+---------------------+
```

It is assumed that within this table, there is a row for each day of the cost modelling period.

A separate table like this will be produced for each line item.

### Converting assumptions into the target data structure

To make the cost model functional, we need a way of defining the assumptions that will drive the model.

Since there are several different types of cost, we will need several different formats for writing down assumptions.

For instance, for an 'annual cost', the format for assumptions can be simple, probably covering just quantity, and price_gbp_real.

A more complex type of assumption could involve

So the general data processing flow is:

assumption format -> target data structure for each assumption line item -> concatenated dataframe -> summary tables and charts.

We will then be able to define other

## Notes

One issue with cost modelling is how to deal with continuous costs like staff costs

If you model staff costs as a weekly payment, then summarize by month, you will get different amounts each month, so your charts will look weird

So we model continuous costs like running costs and staff costs daily

We need a way of interpolating for continuous costs - if you have weekly costs, splitting them to daily etc.  A kind of 'interpolate and split'
