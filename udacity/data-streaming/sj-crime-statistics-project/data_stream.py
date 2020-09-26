import logging
import json
from pyspark.sql import SparkSession
from pyspark.sql.types import *
import pyspark.sql.functions as psf


KAFKA_TOPIC = "org.sfpd.crime.calls"
BOOTSTRAP_SERVER = "localhost:9092"


schema = StructType([
    StructField("crime_id", StringType(), False),
    StructField("original_crime_type_name", StringType(), False),
    StructField("report_date", StringType(), False),
    StructField("call_date", StringType(), False),
    StructField("offense_date", StringType(), False),
    StructField("call_time", StringType(), False),
    StructField("call_date_time", StringType(), False),
    StructField("disposition", StringType(), False),
    StructField("address", StringType(), False),
    StructField("city", StringType(), False),
    StructField("state", StringType(), False),
    StructField("agency_id", StringType(), False),
    StructField("address_type", StringType(), False),
    StructField("common_location", StringType(), False),
])


def run_spark_job(spark):
    # Create Spark configurations with max offset of 200 per trigger
    # set up correct bootstrap server and port
    df = (
        spark 
        .readStream
        .format("kafka")
        .option("subscribe", KAFKA_TOPIC)
        .option("kafka.bootstrap.servers", BOOTSTRAP_SERVER)
        .option("startingOffsets", "earliest")
        .option("maxOffsetsPerTrigger", 200)
        .option("stopGracefullyOnShutdown", "true")
        .load()
    )

    # Show schema for the incoming resources for checks
    df.printSchema()

    # Take only value and convert it to String
    kafka_df = df.selectExpr("CAST(value AS STRING)")

    service_table = kafka_df\
        .select(psf.from_json(psf.col('value'), schema).alias("df"))\
        .select("df.*")
    
    # Check to see if Kafka data has been correctly extracted using the schema
#     query = (
#         service_table 
#         .writeStream 
#         .outputMode('append')
#         .format('console')
#         .start()
#     )
#     query.awaitTermination()

    # select original_crime_type_name and disposition
    distinct_table = (
        service_table
            .select(
                "crime_id", 
                psf.to_timestamp(psf.col('call_date_time')).alias('call_date_time'), 
                "original_crime_type_name", 
                "disposition"
            )
            .distinct()
    )
    
    # count the number of original crime type
    agg_df = distinct_table \
        .withWatermark("call_date_time", "60 minutes") \
        .groupBy(
            psf.window(distinct_table.call_date_time, "10 minutes", "5 minutes"),
            "original_crime_type_name",
        ) \
        .count()

    # TODO Q1. Submit a screen shot of a batch ingestion of the aggregation
    # TODO write output stream
    query = (
        agg_df 
        .writeStream 
        .trigger(processingTime="2 seconds")
        .outputMode('complete')
        .format('console')
        .option("truncate", "false")
        .start()
    )

    query.awaitTermination()

    # TODO get the right radio code json path
    radio_code_json_filepath = "radio_code.json"
    radio_code_df = spark.read.json(radio_code_json_filepath, multiLine=True)
    
    radio_code_df.show()
    radio_code_df.printSchema()
    agg_df.printSchema()

    # clean up your data so that the column names match on radio_code_df and agg_df
    # we will want to join on the disposition code

    # TODO rename disposition_code column to disposition
    radio_code_df = radio_code_df.withColumnRenamed("disposition_code", "disposition")

#     # TODO join on disposition column
#     join_query = agg_df.join(radio_code_df, "disposition", "inner")

#     join_query\
#         .writeStream\
#         .outputMode("complete")\
#         .format("console")\
#         .start()

#     join_query.awaitTermination()


if __name__ == "__main__":
    logger = logging.getLogger(__name__)

    # TODO Create Spark in Standalone mode
    spark = SparkSession \
        .builder \
        .master("local[*]") \
        .config("spark.ui.port", 3000) \
        .appName("KafkaSparkStructuredStreaming") \
        .getOrCreate()

    logger.info("Spark started")

    run_spark_job(spark)

    spark.stop()
    
    # Start script:
    # spark-submit --packages org.apache.spark:spark-sql-kafka-0-10_2.11:2.3.4 data_stream.py