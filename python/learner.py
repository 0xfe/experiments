#!/usr/bin/env python
#
# A library for spaced repitition learning.
#
# Author: Mohit Cheppudira <mohit@muthanna.com>

class LearningException(Exception):
  """Class for exceptions in SM2"""
  pass

class LearningScheduler(object):
  def quality_range(self):
    """Returns the quality range of this scheduler."""
    return range(1, 5)

  def validate_quality(self, quality):
    if quality not in self.quality_range():
      raise LearningException("Quality metric out of range.")

  def schedule(self, quality):
    """
    Schedule the next repitition of this knowledge item
    based on the quality of the previous response. The
    quality parameter is an integer within the quality range
    of this class.

    Returns the number of days after which this item should be
    viewed next. A return value of 0 means that it should be repeated
    again today after the repitition session.
    """
    pass

class MoScheduler(LearningScheduler):
  """
  An implementation of the Mo spaced repitition learning algorithm. Loosely
  based on the SM2 algorithm, documented in:

  http://www.supermemo.com/english/ol/sm2.htm
  """

  INITIAL_EFACTOR = 2.5

  def __init__(self):
    self._efactor = MoScheduler.INITIAL_EFACTOR
    self._repitition = 0
    self._last_interval = 0
    self._last_schedule = -1

  @property
  def efactor(self):
    return self._efactor

  @property
  def repitition(self):
    return self._repitition

  @property
  def last_interval(self):
    return self._last_interval

  @property
  def last_schedule(self):
    return self._last_schedule

  def __str__(self):
    return (
      "efactor = %f\n" +
      "repitition = %d\n" +
      "last_interval = %f\n" +
      "last_schedule = %f") % (self._efactor,
                              self._repitition,
                              self._last_interval,
                              self._last_schedule)

  def quality_range(self):
    """
    The quality range for this scheduler is:

    3 = perfect response
    2 = correct response after hesitation
    1 = correct response recalled with serious difficulty
    0 = incorrect response
    """

    return range(0, 4)

  def schedule(self, quality):
    self.validate_quality(quality)

    if quality < 1:
      """
      If the quality response was lower than 1 then start repetitions for the
      item from the beginning without changing the E-Factor (i.e. use intervals
      I(1), I(2) etc. as if the item was memorized anew).
      """

      self._repitition = 0
      self._last_interval = 1
    else:
      """
      Repeat items using the following intervals:

        I(1) := 1
        I(2) := 4
        for n>2: I(n):=I(n-1)*EF

        where:
        I(n) - inter-repetition interval after the n-th repetition (in days),
        EF - E-Factor of a given item

      If interval is a fraction, round it up to the nearest integer.
      """

      self._repitition = self._repitition + 1

      if self._repitition == 1:
        self._last_interval = 1
      if self._repitition == 2:
        self._last_interval = 4
      if self._repitition > 2:
        self._last_interval = self._last_interval * self._efactor

    """
    After each repetition modify the E-Factor of the recently repeated item
    according to the formula:

      EF':=EF+(0.1-(3-quality)*(0.08+(3-quality)*0.02))

    If EF is less than 1.3 then let EF be 1.3
    """

    self._efactor = self._efactor + (0.1 -
        (3 - quality) * (0.08 + (3 - quality) * 0.02))

    if self._efactor < 1.3:
      self._efactor = 1.3

    if quality < 1:
      """
      After each repetition session of a given day repeat again all items that
      scored below two in the quality assessment. Continue the repetitions
      until all of these items score at least four.
      """
      self._last_schedule = 0
    else:
      self._last_schedule = self._last_interval

    return self._last_schedule

import unittest

class TestMoScheduler(unittest.TestCase):
  @staticmethod
  def process(learner, quality):
    print "Quality = %d" % quality
    learner.schedule(quality)
    print learner
    print

  def testinitial(self):
    print "Initial Test\n---------------\n"
    learner = MoScheduler()
    TestMoScheduler.process(learner, 0)
    self.assertEqual(learner.last_schedule, 0)

  def testlong(self):
    print "Long Test\n---------------\n"
    learner = MoScheduler()

    for quality in learner.quality_range():
      for reps in range(1, 5):
        TestMoScheduler.process(learner, quality)

  def testforgetting(self):
    print "Forgetting Test\n---------------\n"
    learner = MoScheduler()
    TestMoScheduler.process(learner, 0)
    TestMoScheduler.process(learner, 0)
    TestMoScheduler.process(learner, 1)
    TestMoScheduler.process(learner, 1)
    TestMoScheduler.process(learner, 2)
    TestMoScheduler.process(learner, 2)
    TestMoScheduler.process(learner, 3)
    TestMoScheduler.process(learner, 1)
    TestMoScheduler.process(learner, 1)
    TestMoScheduler.process(learner, 2)
    TestMoScheduler.process(learner, 2)
    TestMoScheduler.process(learner, 2)
    TestMoScheduler.process(learner, 2)
    TestMoScheduler.process(learner, 3)

  def testworking(self):
    print "Working Test\n---------------\n"
    learner = MoScheduler()
    TestMoScheduler.process(learner, 0)
    TestMoScheduler.process(learner, 0)
    TestMoScheduler.process(learner, 1)
    TestMoScheduler.process(learner, 1)
    TestMoScheduler.process(learner, 2)
    TestMoScheduler.process(learner, 2)
    TestMoScheduler.process(learner, 3)
    TestMoScheduler.process(learner, 2)
    TestMoScheduler.process(learner, 2)
    TestMoScheduler.process(learner, 2)

if  __name__ == '__main__':
  unittest.main()

