# Finance rules repository: dist_rules, amount_map, routing_rules,
# posting_line_types. Read paths used by the posting engine and cost-centre
# matrix validation. (Seeding lives in R/setup.r's build_rules_tables().)

rules_repository <- function(con) {
  list(
    ruleset_bundle = function(ruleset_id) {
      dist_rules <- DBI::dbGetQuery(con, "
        SELECT scenario_id, row_category, condition_field, condition_op, condition_value,
               posting_line_type_id, priority
        FROM dist_rules
        WHERE ruleset_id = ?
      ", params = list(ruleset_id))

      amount_map <- DBI::dbGetQuery(con, "
        SELECT posting_line_type_id, base_mult, split_mult, applies_to_row_category, calc_method
        FROM amount_map
      ")

      routing_rules <- DBI::dbGetQuery(con, "
        SELECT scenario_id, condition_field, condition_op, condition_value,
               posting_line_type_id, destination_bucket, priority
        FROM routing_rules
        WHERE ruleset_id = ?
      ", params = list(ruleset_id))

      list(
        dist_rules    = dist_rules,
        amount_map    = amount_map,
        routing_rules = routing_rules
      )
    },

    posting_line_type_ids = function() {
      DBI::dbGetQuery(
        con,
        "SELECT posting_line_type_id FROM posting_line_types ORDER BY posting_line_type_id"
      )$posting_line_type_id
    }
  )
}
