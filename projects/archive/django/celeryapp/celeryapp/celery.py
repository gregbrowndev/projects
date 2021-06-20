from __future__ import absolute_import, unicode_literals
import os
from celery import Celery

# set the default Django settings module for the 'celery' program.
from celery.canvas import signature
from celery.schedules import crontab

os.environ.setdefault('DJANGO_SETTINGS_MODULE', 'celeryapp.settings')

app = Celery('celeryapp')

# Using a string here means the worker doesn't have to serialize
# the configuration object to child processes.
# - namespace='CELERY' means all celery-related configuration keys
#   should have a `CELERY_` prefix.
app.config_from_object('django.conf:settings', namespace='CELERY')

# Load task modules from all registered Django app configs.
app.autodiscover_tasks()

@app.task(bind=True)
def debug_task(self):
    print('Request: {0!r}'.format(self.request))


# @app.on_after_configure.connect
# def setup_periodic_tasks(sender, **kwargs):
#
#     sender.add_periodic_task(
#         30.0,
#         signature('main.tasks.add', args=(3,7)),
#         name='add_task_30s'
#     )
#
#     sender.add_periodic_task(
#         crontab(),
#         signature('main.tasks.mul', args=(3, 7)),
#         name='mul_task_minute'
#     )

# There doesn't seem to be much advantage to using the above 'setup_periodic_tasks' method, unless you can figure out
# how to move periodic task declarations into the apps for better encapsulation... There is this alternative below, or
# define CELERY_BEAT_SCHEDULE in the settings.py module.

app.conf.beat_schedule = {
    'add_task_30s': {
        'task': 'main.tasks.add',
        'schedule':  30.0,
        'args': (3, 2)
    },

    'mul_task_every_minute': {
        'task': 'main.tasks.mul',
        'schedule': crontab(),
        'args': (3, 2)
    }
}

# Could also combine both methods, i.e.
# in settings/base.py:
# CELERY_BEAT_SCHEDULE = {}

# in settings/platform.py
# CELERY_BEAT_SCHEDULE = {
#    # platform-specific periodic tasks, e.g. testing or production only
# }

# in toplevel/celery.py:
# app.conf.beat_schedule.update({
#     # common periodic tasks
#  })