
import pandas as pd
import networkx as nx
import matplotlib.pyplot as plt
import numpy as np
from scipy.stats import wasserstein_distance


def create_multilayer_infrastructure_network_from_data():
    try:
        nodes_df = pd.read_csv("nodes_sample_100.csv")
        edges_df = pd.read_csv("edges_sample_100.csv")
    except FileNotFoundError as e:
        print(f"Error: {e}. Please run the R script first to generate the input files.")
        return None, None

    print(f"Loaded {len(nodes_df)} nodes and {len(edges_df)} edges.")

    G = nx.Graph()
    for x, row in nodes_df.iterrows():
        G.add_node(row['actor'], layer=row['layer'], lat=row['latitude'], lon=row['longitude'])

    for y, row in edges_df.iterrows():
        G.add_edge(row['actor_from'], row['actor_to'])

    print(f"Network created with {G.number_of_nodes()} nodes and {G.number_of_edges()} edges.\n")
    return G, nodes_df

def visualize_network(G, title="Infrastructure Network"):

    plt.figure(figsize=(20, 20))

    pos = {node: (d['lon'], d['lat']) for node, d in G.nodes(data=True)}

    nx.draw(G, pos, with_labels=False, node_size=50, width=0.5)

    plt.title(title)
    plt.show()


def run_tsr_simulation():
    G, nodes_df = create_multilayer_infrastructure_network_from_data()
    if G is None:
        return

    visualize_network(G, "Initial Multi-layer Infrastructure Network")

    baseline_deg = np.array([d for i, d in G.degree()])
    wasserstein_distances = []
    for frac in np.linspace(0, .5, 6):
        Gp = G.copy()
        k = int(frac * Gp.number_of_nodes())
        to_remove = np.random.choice(list(Gp.nodes()), size=k, replace=False)
        Gp.remove_nodes_from(to_remove)

        pert_deg = np.array([d for _, d in Gp.degree()])
        dist = wasserstein_distance(baseline_deg, pert_deg)
        wasserstein_distances.append(dist)

    finite_ds = [d for d in wasserstein_distances if np.isfinite(d)]
    if not finite_ds:
        print("Error, No distances computed")
        tsr_metric = np.inf
    else:
        tsr_metric = np.mean(finite_ds)

    print("====== TSR Simulation Complete ======")
    print(f"Topology-based Statistical Robustness (TSR) Metric: {tsr_metric:.4f}")
    return tsr_metric


if __name__ == "__main__":
    run_tsr_simulation()
