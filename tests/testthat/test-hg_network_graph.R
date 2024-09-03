test_that("hg_network_graph", {
  df <- data.frame(
    pais = rep("Colombia", 32),
    region = c(
      "Amazonas", "Amazonas", "Andina", "Andina", "Andina", "Andina", "Andina",
      "Andina", "Andina", "Andina", "Andina", "Caribe", "Caribe", "Caribe",
      "Caribe", "Caribe", "Caribe", "Caribe", "Caribe", "Pacífica", "Pacífica",
      "Pacífica", "Pacífica", "Pacífica", "Orinoquía", "Orinoquía", "Orinoquía",
      "Orinoquía", "Orinoquía", "Insular", "Pacífica", "Pacífica"
    ),
    departamento = c(
      "Amazonas", "Guaviare", "Antioquia", "Boyacá", "Caldas", "Cundinamarca",
      "Huila", "Quindío", "Risaralda", "Santander", "Tolima", "Atlántico",
      "Bolívar", "Cesar", "Córdoba", "La Guajira", "Magdalena",
      "San Andrés y Providencia", "Sucre", "Cauca", "Chocó", "Nariño",
      "Valle del Cauca", "Caquetá", "Arauca", "Casanare", "Guainía", "Meta",
      "Vaupés","San Andrés y Providencia", "Nariño", "Valle del Cauca"
    )
  )

  hg_network_graph(df, var_cat = c("pais", "region", "departamento"))
})

test_that("hg_network_graph_CatCat", {
  data <- data.frame(
    parent = c("CEO", "CEO", "Manager1", "Manager1", "Manager2", "Manager2", "TeamLead1", "TeamLead2"),
    child = c("Manager1", "Manager2", "TeamLead1", "TeamLead2", "TeamLead3", "TeamLead4", "Employee1", "Employee2")
  )

  hg_network_graph_CatCat(data)
})
