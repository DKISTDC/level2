export GRAPHQL_API="http://internal-api-gateway.service.sim.consul/graphql"
export DIR="deps"
echo "Generating metadata schema from $GRAPHQL_API"
npx get-graphql-schema $GRAPHQL_API >$DIR/metadata.graphql
echo "Saved to $DIR/metadata.graphql"
