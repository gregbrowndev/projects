"""Add station table

Revision ID: 0d5e9fccac4d
Revises: 37515f7ee188
Create Date: 2018-08-18 22:23:53.653578

"""
from alembic import op
import sqlalchemy as sa


# revision identifiers, used by Alembic.
revision = '0d5e9fccac4d'
down_revision = '37515f7ee188'
branch_labels = None
depends_on = None


def upgrade():
    # ### commands auto generated by Alembic - please adjust! ###
    op.create_table('station',
    sa.Column('id', sa.Integer(), nullable=False),
    sa.Column('source_id', sa.String(), nullable=True),
    sa.Column('created', sa.DateTime(), nullable=True),
    sa.Column('modified', sa.DateTime(), nullable=True),
    sa.Column('name', sa.String(), nullable=False),
    sa.Column('latitude', sa.Float(), nullable=False),
    sa.Column('longitude', sa.Float(), nullable=False),
    sa.Column('address', sa.String(), nullable=False),
    sa.Column('capacity', sa.Integer(), nullable=True),
    sa.Column('bikes_available', sa.Integer(), nullable=True),
    sa.Column('docks_available', sa.Integer(), nullable=True),
    sa.Column('bikes_disabled', sa.Integer(), nullable=True),
    sa.Column('docks_disabled', sa.Integer(), nullable=True),
    sa.Column('open', sa.Boolean(), nullable=True),
    sa.Column('system_id', sa.Integer(), nullable=False),
    sa.ForeignKeyConstraint(['system_id'], ['system.id'], ondelete='CASCADE'),
    sa.PrimaryKeyConstraint('id')
    )
    op.add_column('system', sa.Column('modified', sa.DateTime(), nullable=True))
    # ### end Alembic commands ###


def downgrade():
    # ### commands auto generated by Alembic - please adjust! ###
    op.drop_column('system', 'modified')
    op.drop_table('station')
    # ### end Alembic commands ###
