# Create your tasks here
from __future__ import absolute_import, unicode_literals
from celery import shared_task, current_app
from celery.schedules import crontab

# app = current_app()

@shared_task
def add(x, y):
    return x + y


@shared_task
def mul(x, y):
    return x * y


@shared_task
def xsum(numbers):
    return sum(numbers)


# Add some periodic tasks  -  can't seem to get this to work. It seems the 'centralised', put everything in celery.py
# approach is the easiest to work with.

# @app.on_after_configure.connect
# def setup_periodic_tasks(sender, **kwargs):
#
#     sender.add_periodic_task(
#         10.0,
#         mul.s(3,7),
#         name='mul_task_10s'
#     )

