ELASTIC_URL='http://localhost:9200'
INDEX_NAME='people'

echo "Delete index..."
curl -XDELETE $ELASTIC_URL/$INDEX_NAME/

echo "\nPut index..."
curl -XPUT $ELASTIC_URL/$INDEX_NAME/ -d @workshop_settings.json

echo "\nPost mappings..."
curl -XPOST $ELASTIC_URL/$INDEX_NAME/star/_mapping -d @workshop_mapping.json

echo "\nIndexing the corpus..."
curl -XPOST $ELASTIC_URL/_bulk --data-binary @corpus.jsonl | python -m json.tool

echo "\nFinished."