import urllib
import boto3
import string
from subprocess import Popen, PIPE

s3 = boto3.client('s3')

def lambda_handler(event, context):
    bucket = event['Records'][0]['s3']['bucket']['name']
    key = urllib.unquote_plus(event['Records'][0]['s3']['object']['key'].encode('utf8'))

    try:
        response = s3.get_object(Bucket=bucket, Key=key)
        targetBucket = string.replace(bucket, "-quarantine", "")
        body = response['Body'].read()
        contentType = response['ContentType']
        outputFormat = output_format(contentType)

        cmdHead = [
                    'convert',  # ImageMagick Convert
                    '-',        # Read original picture from StdIn
                    '-strip'    # Remove metadata
                  ]
        cmdTail = [
                    'jpeg:-' # Write output with to StdOut
                  ]

        cmd = cmdHead + maybe_limit_size(len(body)) + cmdTail

        p = Popen(cmd, stdout=PIPE, stdin=PIPE)
        cleanImage = p.communicate(input=body)[0]

        s3.put_object(Bucket = targetBucket,
                      Key = key,
                      Body = cleanImage,
                      Metadata = response['Metadata'],
                      ContentType = "image/jpeg")
        s3.delete_object(Bucket = bucket,
                         Key = key)

    except Exception as e:
        print(e)
        print('Error getting object {} from bucket {}. Make sure they exist and your bucket is in the same region as this function.'.format(key, bucket))
        raise e

def output_format(contentType):
    if contentType == "image/png":
        return "png"
    elif contentType == "image/jpeg":
        return "jpeg"
    else:
        raise Exception('Unhandled content type: {}'.contentType)

def maybe_limit_size(size):
    if size > (1024 * 1024):  # 1MB limit
        return ['-define', 'jpeg:extent=1024KB']
    else:
        return []
