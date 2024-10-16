"""Add scraper table

Revision ID: eaf1e1b72ab9
Revises: 0d5e9fccac4d
Create Date: 2018-08-19 02:02:06.488971

"""
from alembic import op
import sqlalchemy as sa


# revision identifiers, used by Alembic.
revision = 'eaf1e1b72ab9'
down_revision = '0d5e9fccac4d'
branch_labels = None
depends_on = None


def upgrade():
    # ### commands auto generated by Alembic - please adjust! ###
    op.create_table('scraper',
    sa.Column('created', sa.DateTime(), nullable=True),
    sa.Column('modified', sa.DateTime(), nullable=True),
    sa.Column('id', sa.Integer(), nullable=False),
    sa.Column('name', sa.String(), nullable=False),
    sa.PrimaryKeyConstraint('id'),
    sa.UniqueConstraint('name')
    )
    op.add_column('station', sa.Column('scraper_id', sa.Integer(), nullable=False))
    op.create_unique_constraint('station_scraper_id_source_id_key', 'station', ['scraper_id', 'source_id'])
    op.create_foreign_key(None, 'station', 'scraper', ['scraper_id'], ['id'], ondelete='CASCADE')
    op.add_column('system', sa.Column('scraper_id', sa.Integer(), nullable=False))
    op.create_unique_constraint('system_scraper_id_source_id_key', 'system', ['scraper_id', 'source_id'])
    op.create_foreign_key(None, 'system', 'scraper', ['scraper_id'], ['id'], ondelete='CASCADE')
    # ### end Alembic commands ###


def downgrade():
    # ### commands auto generated by Alembic - please adjust! ###
    op.drop_constraint(None, 'system', type_='foreignkey')
    op.drop_constraint('system_scraper_id_source_id_key', 'system', type_='unique')
    op.drop_column('system', 'scraper_id')
    op.drop_constraint(None, 'station', type_='foreignkey')
    op.drop_constraint('station_scraper_id_source_id_key', 'station', type_='unique')
    op.drop_column('station', 'scraper_id')
    op.drop_table('scraper')
    # ### end Alembic commands ###
