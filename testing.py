# -*- coding: utf-8 -*-
"""
Created on Sat Dec  2 01:55:32 2017

@author: Fernando
"""

# Import TensorFlow
#import tensorflow as tf
#
#
## Define a and b as placeholders
#a = tf.placeholder(dtype=tf.int8)
#b = tf.placeholder(dtype=tf.int8)
#
### Define the addition
#c = tf.add(a, b)
## Initialize the graph
#sess = tf.InteractiveSession()
#
### Run the graph
#sess.run(c, feed_dict={a: 5, b: 4})
#
###Run a session
##sess = tf.InteractiveSession()
##c = tf.add(5,4)
##sess.run(c)

# Enter an interactive TensorFlow Session.
import tensorflow as tf
sess = tf.InteractiveSession()

x = tf.Variable([1.0, 2.0])
a = tf.constant([3.0, 3.0])

# Initialize 'x' using the run() method of its initializer op.
x.initializer.run()

# Add an op to subtract 'a' from 'x'.  Run it and print the result
sub = tf.subtract(x, a)
print(sub.eval())
# ==> [-2. -1.]

# Close the Session when we're done.
sess.close()