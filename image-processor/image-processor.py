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

        cmdHead = [
                    'convert',  # ImageMagick Convert
                    '-',        # Read original picture from StdIn
                    '-strip'    # Remove metadata
                  ]
        cmdTail = [
                    output_format(contentType) + ':-' # Write output with to StdOut
                  ]

        # Make full size clean image
        cmd = cmdHead + limit_size() + cmdTail
        p = Popen(cmd, stdout=PIPE, stdin=PIPE)
        cleanImage = p.communicate(input=body)[0]

        s3.put_object(Bucket = targetBucket,
                      Key = key,
                      Body = cleanImage,
                      Metadata = response['Metadata'],
                      ContentType = contentType)

        # Make thumbnail
        cmd = cmdHead + thumbnail_params() + cmdTail
        p = Popen(cmd, stdout=PIPE, stdin=PIPE)
        thumbnailImage = p.communicate(input=body)[0]

        s3.put_object(Bucket = targetBucket,
                      Key = key + "-thumbnail",
                      Body = thumbnailImage,
                      Metadata = response['Metadata'],
                      ContentType = contentType)

        # Copy over the original image (in case we want to reprocess it in the
        # future)
        s3.put_object(Bucket = targetBucket,
                      Key = key + "-original",
                      Body = body,
                      Metadata = response['Metadata'],
                      ContentType = contentType)

        # Clean up source object
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

def limit_size():
    # Max size of an iPhone 7+'s largest axis - do not grow
    return ['-resize', '1920x1920>']

def thumbnail_params():
    # 1/3 the width of iPhone7+, in a square, cropped
    thumb_size = '360x360'
    return ['-thumbnail', thumb_size + "^",
            '-gravity', 'center',
            '-extent', thumb_size]
