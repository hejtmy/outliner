import React, { useCallback, useEffect, useMemo, useState } from 'react';
import { Background, Controls, MiniMap, MarkerType, ReactFlow, useEdgesState, useNodesState } from '@xyflow/react';
import '@xyflow/react/dist/style.css';
import dagre from 'dagre';
import CustomNode from './CustomNode';
import './App.css';

const nodeTypes = {
  step: CustomNode,
  artifact: CustomNode,
};

const PALETTE = ['#0f766e', '#9a3412', '#7c3aed', '#2563eb', '#b45309', '#be123c'];

const normalizeGraph = (payload) => ({
  nodes: Array.isArray(payload?.nodes) ? payload.nodes : [],
  edges: Array.isArray(payload?.edges) ? payload.edges : [],
  meta: {
    title: payload?.meta?.title || 'Outliner',
    root_file: payload?.meta?.root_file || '',
    tracked_objects: Array.isArray(payload?.meta?.tracked_objects) ? payload.meta.tracked_objects : [],
    documents: Array.isArray(payload?.meta?.documents)
      ? payload.meta.documents
      : Object.values(payload?.meta?.documents || {}),
  },
});

const getLayoutedElements = (nodes, edges) => {
  const graph = new dagre.graphlib.Graph();
  graph.setDefaultEdgeLabel(() => ({}));
  graph.setGraph({ rankdir: 'LR', nodesep: 28, ranksep: 90, marginx: 24, marginy: 24 });

  nodes.forEach((node) => {
    graph.setNode(node.id, {
      width: node.type === 'step' ? 264 : 176,
      height: node.type === 'step' ? 118 : 62,
    });
  });

  edges.forEach((edge) => {
    graph.setEdge(edge.source, edge.target);
  });

  dagre.layout(graph);

  return {
    nodes: nodes.map((node) => {
      const positioned = graph.node(node.id);
      const width = node.type === 'step' ? 264 : 176;
      const height = node.type === 'step' ? 118 : 62;
      return {
        ...node,
        position: {
          x: positioned.x - width / 2,
          y: positioned.y - height / 2,
        },
        sourcePosition: 'right',
        targetPosition: 'left',
      };
    }),
    edges,
  };
};

const assignComponentAccents = (nodes, edges) => {
  const adjacency = {};
  const colored = nodes.map((node) => ({ ...node, data: { ...node.data } }));

  colored.forEach((node) => {
    adjacency[node.id] = [];
  });
  edges.forEach((edge) => {
    adjacency[edge.source]?.push(edge.target);
    adjacency[edge.target]?.push(edge.source);
  });

  const visited = new Set();
  let paletteIndex = 0;
  const byId = Object.fromEntries(colored.map((node) => [node.id, node]));

  colored.forEach((node) => {
    if (visited.has(node.id)) {
      return;
    }

    const accent = PALETTE[paletteIndex % PALETTE.length];
    paletteIndex += 1;
    const queue = [node.id];
    visited.add(node.id);

    while (queue.length > 0) {
      const current = queue.shift();
      if (byId[current]) {
        byId[current].data.accent = accent;
      }

      adjacency[current]?.forEach((neighbor) => {
        if (!visited.has(neighbor)) {
          visited.add(neighbor);
          queue.push(neighbor);
        }
      });
    }
  });

  return colored;
};

const styleEdges = (edges) =>
  edges.map((edge) => {
    const kind = edge.data?.kind || 'produces';
    const base = {
      ...edge,
      animated: kind === 'sequence',
      type: kind === 'sequence' ? 'smoothstep' : 'default',
      markerEnd: {
        type: MarkerType.ArrowClosed,
        width: 18,
        height: 18,
      },
      style: {
        strokeWidth: kind === 'sequence' ? 1.8 : 2.3,
        stroke: kind === 'sequence' ? '#7c6f64' : '#3f3a34',
        strokeDasharray: kind === 'sequence' ? '6 6' : 'none',
      },
    };

    return base;
  });

const buildLineageSet = (nodes, edges, artifactId) => {
  if (!artifactId) {
    return new Set(nodes.map((node) => node.id));
  }

  const forward = {};
  const backward = {};
  nodes.forEach((node) => {
    forward[node.id] = [];
    backward[node.id] = [];
  });
  edges.forEach((edge) => {
    forward[edge.source]?.push(edge.target);
    backward[edge.target]?.push(edge.source);
  });

  const visible = new Set([artifactId]);
  const stack = [artifactId];

  while (stack.length > 0) {
    const current = stack.pop();
    [...(forward[current] || []), ...(backward[current] || [])].forEach((neighbor) => {
      if (!visible.has(neighbor)) {
        visible.add(neighbor);
        stack.push(neighbor);
      }
    });
  }

  return visible;
};

const formatLocation = (source) => {
  if (!source?.file) {
    return null;
  }
  const line = source.line_start ? `:${source.line_start}` : '';
  const chunk = source.chunk ? ` (${source.chunk})` : '';
  return `${source.file}${line}${chunk}`;
};

const summarizeDetails = (details) => (Array.isArray(details) ? details.filter(Boolean) : []);

export default function App() {
  const [graph, setGraph] = useState({ nodes: [], edges: [], meta: { documents: [], tracked_objects: [] } });
  const [selectedId, setSelectedId] = useState(null);
  const [focusArtifactId, setFocusArtifactId] = useState(null);
  const [viewMode, setViewMode] = useState('all');
  const [nodes, setNodes, onNodesChange] = useNodesState([]);
  const [edges, setEdges, onEdgesChange] = useEdgesState([]);

  useEffect(() => {
    const load = async () => {
      let payload = { nodes: [], edges: [], meta: {} };

      if (window.outlinerData) {
        payload = window.outlinerData;
      } else {
        try {
          const response = await fetch('/data.json');
          if (response.ok) {
            payload = await response.json();
          }
        } catch (error) {
          console.error('Failed to load outliner data', error);
        }
      }

      const normalized = normalizeGraph(payload);
      setGraph(normalized);

      if (normalized.meta.tracked_objects.length > 0) {
        const firstTrackedId = `artifact:${normalized.meta.tracked_objects[0].replace(/[^A-Za-z0-9_.:-]/g, '_')}`;
        setFocusArtifactId(firstTrackedId);
        setViewMode('lineage');
      }
    };

    load();
  }, []);

  const preparedGraph = useMemo(() => {
    const coloredNodes = assignComponentAccents(graph.nodes, graph.edges);
    const laidOut = getLayoutedElements(coloredNodes, styleEdges(graph.edges));
    return laidOut;
  }, [graph]);

  const visibleIds = useMemo(
    () => (viewMode === 'lineage' ? buildLineageSet(preparedGraph.nodes, preparedGraph.edges, focusArtifactId) : new Set(preparedGraph.nodes.map((node) => node.id))),
    [preparedGraph, viewMode, focusArtifactId],
  );

  const visibleGraph = useMemo(() => ({
    nodes: preparedGraph.nodes.filter((node) => visibleIds.has(node.id)),
    edges: preparedGraph.edges.filter((edge) => visibleIds.has(edge.source) && visibleIds.has(edge.target)),
  }), [preparedGraph, visibleIds]);

  useEffect(() => {
    setNodes(visibleGraph.nodes);
    setEdges(visibleGraph.edges);
  }, [visibleGraph, setNodes, setEdges]);

  const selectedNode = useMemo(
    () => preparedGraph.nodes.find((node) => node.id === selectedId) || null,
    [preparedGraph.nodes, selectedId],
  );

  const stepRegistry = useMemo(
    () => visibleGraph.nodes.filter((node) => node.type === 'step'),
    [visibleGraph.nodes],
  );

  const artifactRegistry = useMemo(
    () => preparedGraph.nodes.filter((node) => node.type === 'artifact').sort((a, b) => a.data.label.localeCompare(b.data.label)),
    [preparedGraph.nodes],
  );

  const selectNode = useCallback((nodeId) => {
    setSelectedId(nodeId);
  }, []);

  const onNodeClick = useCallback((_, node) => {
    setSelectedId(node.id);
    if (node.type === 'artifact') {
      setFocusArtifactId(node.id);
    }
  }, []);

  const clearFocus = useCallback(() => {
    setFocusArtifactId(null);
    setViewMode('all');
  }, []);

  const selectedLocation = selectedNode ? formatLocation(selectedNode.data.source || selectedNode.data.call_source) : null;
  const definitionLocation = selectedNode ? formatLocation(selectedNode.data.definition_source) : null;
  const detailItems = selectedNode ? summarizeDetails(selectedNode.data.details) : [];

  return (
    <div className="app-shell">
      <aside className="sidebar">
        <div className="sidebar-header">
          <p className="eyebrow">Outliner</p>
          <h1>{graph.meta.title || 'Analysis outline'}</h1>
          <p className="sidebar-copy">Trace annotated steps, inspect function internals, and focus on one artifact lineage when you need a publishable narrative.</p>
        </div>

        <div className="mode-switch">
          <button className={viewMode === 'all' ? 'active' : ''} onClick={() => setViewMode('all')}>Full graph</button>
          <button className={viewMode === 'lineage' ? 'active' : ''} onClick={() => setViewMode('lineage')}>Lineage</button>
        </div>

        <section className="sidebar-section">
          <div className="section-header">
            <h2>Artifacts</h2>
            {focusArtifactId && <button className="ghost-button" onClick={clearFocus}>Clear focus</button>}
          </div>
          <div className="artifact-list">
            {artifactRegistry.map((artifact) => (
              <button
                key={artifact.id}
                className={`artifact-chip ${focusArtifactId === artifact.id ? 'is-focused' : ''}`}
                onClick={() => {
                  setFocusArtifactId(artifact.id);
                  setViewMode('lineage');
                  selectNode(artifact.id);
                }}
              >
                {artifact.data.label}
              </button>
            ))}
          </div>
        </section>

        <section className="sidebar-section grow">
          <div className="section-header">
            <h2>Steps</h2>
            <span>{stepRegistry.length}</span>
          </div>
          <div className="step-list">
            {stepRegistry.map((step) => (
              <button key={step.id} className={`step-card ${selectedId === step.id ? 'is-selected' : ''}`} onClick={() => selectNode(step.id)}>
                <span className="step-card-kicker">{step.data.kind.replace(/_/g, ' ')}</span>
                <strong>{step.data.label}</strong>
                {step.data.summary && <span>{step.data.summary}</span>}
                {formatLocation(step.data.call_source || step.data.source) && (
                  <small>{formatLocation(step.data.call_source || step.data.source)}</small>
                )}
              </button>
            ))}
          </div>
        </section>
      </aside>

      <main className="canvas-shell">
        <div className="canvas-toolbar">
          <div>
            <p className="eyebrow">Documents</p>
            <div className="document-row">
              {graph.meta.documents.map((doc) => (
                <span key={doc.file} className="document-chip">{doc.title}</span>
              ))}
            </div>
          </div>
          {focusArtifactId && (
            <div className="focus-summary">
              <span>Focused artifact</span>
              <strong>{artifactRegistry.find((artifact) => artifact.id === focusArtifactId)?.data.label}</strong>
            </div>
          )}
        </div>

        <div className="canvas-frame">
          <ReactFlow
            nodes={nodes}
            edges={edges}
            onNodesChange={onNodesChange}
            onEdgesChange={onEdgesChange}
            nodeTypes={nodeTypes}
            onNodeClick={onNodeClick}
            onPaneClick={() => setSelectedId(null)}
            fitView
            fitViewOptions={{ padding: 0.18 }}
          >
            <Controls />
            <MiniMap pannable zoomable nodeColor={(node) => node.data?.accent || '#a8a29e'} maskColor="rgba(247, 244, 236, 0.72)" />
            <Background variant="dots" gap={20} size={1.1} color="#d6d0c4" />
          </ReactFlow>
          {visibleGraph.nodes.length === 0 && <div className="empty-state">No outlineable steps were found in the current view.</div>}
        </div>
      </main>

      <aside className="detail-panel">
        {!selectedNode && (
          <div className="detail-placeholder">
            <p className="eyebrow">Selection</p>
            <h2>Pick a step or artifact</h2>
            <p>The detail panel shows summaries, code excerpts, and source locations for whatever you select.</p>
          </div>
        )}

        {selectedNode && (
          <div className="detail-content">
            <p className="eyebrow">{selectedNode.type === 'step' ? 'Step' : 'Artifact'}</p>
            <h2>{selectedNode.data.label}</h2>
            {selectedNode.data.summary && <p className="detail-summary">{selectedNode.data.summary}</p>}

            <div className="detail-meta">
              {selectedNode.data.function_name && <span>Function: {selectedNode.data.function_name}</span>}
              {selectedLocation && <span>{selectedLocation}</span>}
              {definitionLocation && <span>Definition: {definitionLocation}</span>}
            </div>

            {detailItems.length > 0 && (
              <section className="detail-section">
                <h3>Narrative</h3>
                <ul>
                  {detailItems.map((item, index) => <li key={index}>{item}</li>)}
                </ul>
              </section>
            )}

            {selectedNode.type === 'step' && (
              <>
                <section className="detail-section grid">
                  <div>
                    <h3>Inputs</h3>
                    <p>{[].concat(selectedNode.data.inputs || []).filter(Boolean).join(', ') || 'None recorded'}</p>
                  </div>
                  <div>
                    <h3>Outputs</h3>
                    <p>{[].concat(selectedNode.data.outputs || []).filter(Boolean).join(', ') || 'None recorded'}</p>
                  </div>
                  <div>
                    <h3>Modifies</h3>
                    <p>{[].concat(selectedNode.data.modifies || []).filter(Boolean).join(', ') || 'No explicit modification list'}</p>
                  </div>
                  <div>
                    <h3>Kind</h3>
                    <p>{selectedNode.data.kind.replace(/_/g, ' ')}</p>
                  </div>
                </section>

                <section className="detail-section">
                  <h3>Code</h3>
                  <pre>{selectedNode.data.code || selectedNode.data.invocation_code || 'No code excerpt available.'}</pre>
                </section>

                {selectedNode.data.invocation_code && selectedNode.data.invocation_code !== selectedNode.data.code && (
                  <section className="detail-section">
                    <h3>Invocation</h3>
                    <pre>{selectedNode.data.invocation_code}</pre>
                  </section>
                )}
              </>
            )}
          </div>
        )}
      </aside>
    </div>
  );
}
