import dash
import plotly.graph_objs as go
from dash import dcc, html
from dash.dependencies import Input, Output


def launch_plot(data):
    app = dash.Dash(__name__)

    classes = sorted(set(k[0] for k in data.keys()))
    layer_nums = sorted(set(k[1] for k in data.keys()))
    init_types = sorted(set(k[2] for k in data.keys()))

    app.layout = html.Div(
        [
            html.Div(
                [
                    html.Label("Select Classes:"),
                    dcc.Checklist(
                        id="cls-checklist",
                        options=[{"label": name, "value": name} for name in classes],
                        value=classes,
                        inline=True,
                    ),
                ]
            ),
            html.Div(
                [
                    html.Label("Select Initialization Types:"),
                    dcc.Checklist(
                        id="init-type-checklist",
                        options=[
                            {"label": type_, "value": type_} for type_ in init_types
                        ],
                        value=init_types,
                        inline=True,
                    ),
                ]
            ),
            html.Div(
                [
                    html.Label("Select Layer Nums:"),
                    dcc.Checklist(
                        id="layer-nums-checklist",
                        options=[
                            {"label": layer_num, "value": layer_num}
                            for layer_num in layer_nums
                        ],
                        value=layer_nums,
                        inline=True,
                    ),
                ]
            ),
            dcc.Graph(id="3d-graph"),
        ]
    )

    @app.callback(
        Output("3d-graph", "figure"),
        [
            Input("cls-checklist", "value"),
            Input("init-type-checklist", "value"),
            Input("layer-nums-checklist", "value"),
        ],
    )
    def update_graph(selected_classes, selected_init_types, selected_layer_nums):
        traces = []
        for (cls_name, layer_num, init_type), (losses, accuracies) in data.items():
            if (
                cls_name in selected_classes
                and init_type in selected_init_types
                and layer_num in selected_layer_nums
            ):
                epochs = list(range(len(losses)))
                trace = go.Scatter3d(
                    x=losses,
                    y=accuracies,
                    z=epochs,
                    mode="lines",
                    marker=dict(size=4),
                    line=dict(width=2),
                    name=f"{cls_name} L{layer_num} {init_type}",
                )
                traces.append(trace)

        layout = go.Layout(
            title="Losses and Accuracies Over Epochs",
            scene=dict(
                xaxis=dict(title="Loss"),
                yaxis=dict(title="Accuracy"),
                zaxis=dict(title="Epoch"),
            ),
            margin=dict(l=0, r=0, b=0, t=50),
        )

        return {"data": traces, "layout": layout}

    app.run_server(debug=True)
